from gurobipy import *
import pandas as pd

def schedule_jobs(df):
    # Create a new model
    m = Model("schedule")

    # Create a list of jobs and the maximum possible number of machines
    jobs = df.index.tolist()
    max_machines = len(df)

    # Create variables
    x = m.addVars(jobs, range(max_machines), vtype=GRB.BINARY, name="x")
    y = m.addVars(range(max_machines), vtype=GRB.BINARY, name="y")

    # Constraints: each job is scheduled exactly once
    for i in jobs:
        m.addConstr(quicksum(x[i,k] for k in range(max_machines)) == 1, f"job_{i}")

    # Constraints: if a job is scheduled on a machine, then the machine is used
    for i in jobs:
        for k in range(max_machines):
            m.addConstr(x[i,k] <= y[k], f"machine_{k}_job_{i}")

    # Constraints: no two jobs that overlap are scheduled on the same machine
    for i in jobs:
        for j in jobs:
            if i != j and df.loc[i, "end_time"] > df.loc[j, "starting_time"] and df.loc[j, "end_time"] > df.loc[i, "starting_time"]:
                for k in range(max_machines):
                    m.addConstr(x[i,k] + x[j,k] <= 1, f"overlap_{i}_{j}_{k}")

    # Objective: minimize the total number of machines used
    m.setObjective(quicksum(y[k] for k in range(max_machines)), GRB.MINIMIZE)

    # Solve the model
    m.optimize()

    # Check if a feasible solution was found
    if m.status == GRB.Status.OPTIMAL:
        # Add machine assignments to the DataFrame
        df["machine"] = [None]*len(df)
        for i in jobs:
            for k in range(max_machines):
                if x[i,k].x > 0.5:  # job i is assigned to machine k
                    df.loc[i, "machine"] = k
        return df
    else:
        return None


# Read data from family.csv
family = pd.read_csv("family.csv", encoding="latin1")

# Select relevant columns
data = family[["id", "birth", "death"]]
# if death is missing, set it to 1960, which is the end of the book
data["death"] = data["death"].fillna(1960)

# rename the columns to starting_time and end_time
data = data.rename(columns={"birth": "starting_time", "death": "end_time"})

# filter out the rows with missing values
data = data.dropna().reset_index(drop=True)

# Convert the columns to integers
data["starting_time"] = data["starting_time"].astype(int)
data["end_time"] = data["end_time"].astype(int)

# Optimize the Gantt chart
gantt_order = schedule_jobs(data)
gantt_order.to_csv("gantt_order.csv", index=False)
