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

        # Count the number of machines used
        num_machines = len([y[k] for k in range(max_machines) if y[k].x > 0.5])

        return df, num_machines
    else:
        return None, None


# Read data from family.csv
family = pd.read_csv("../data/family.csv", encoding="latin1")
# Select the relevant columns
data = family[["id", "birth", "death"]].copy()
# if death is missing, set it to 1960, which is the end of the book
data["death"] = data["death"].fillna(1960)

# rename the columns to starting_time and end_time
data = data.rename(columns={"birth": "starting_time", "death": "end_time"})

# filter out the rows with missing values
data = data.dropna().reset_index(drop=True)

# Convert the columns to integers
data["starting_time"] = data["starting_time"].astype(int)
data["end_time"] = data["end_time"].astype(int)

# add some slack the end time
data["end_time"] = data["end_time"] + 2

# Optimize the Gantt chart
gantt_order, num_machines = schedule_jobs(data)
if gantt_order is not None:
    gantt_order.to_csv("../data/gantt_order.csv", index=False)
    print(f"The Gantt chart is saved as gantt_order.csv - used {num_machines} machines")
else:
    print("No feasible solution found")