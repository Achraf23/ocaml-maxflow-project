import random

def generate_data():
    data = []
    options = ["hacker", "host"]
    
    # Counter variables for hackers and hosts
    num_hackers = 0
    num_hosts = 0

    num_lines = random.randint(9, 10)

    for i in range(1, num_lines + 1):
        # Randomly choose between hacker and host based on the specified probabilities
        choice = random.choices(options, weights=[0.6, 0.4], k=1)[0]  # 60% chance for hacker

        if choice == "hacker":
            num_hackers += 1  # Increment hacker count
            # For hacker, first two values should have at least one 'yes', and the third is 50% yes, 50% no
            values = ["yes" if random.random() < 0.6 else "no" for _ in range(2)]
            while values[0] == "no" and values[1] == "no":  # Ensure at least one 'yes' for the first two values
                values = ["yes" if random.random() < 0.6 else "no" for _ in range(2)]
            
            # Append 50% yes/no for the sex parameter
            values.append("yes" if random.random() < 0.5 else "no")
            
            # Append remaining three parameters
            values.extend(["yes" if random.random() < 0.6 else "no" for _ in range(3)])
            
            data.append(f"{i}: hacker: {', '.join(values)}")
        else:
            num_hosts += 1  # Increment host count
            # For host, first two parameters are between 1 and 5, rest are yes or no randomly, including the new one
            host_first = str(random.randint(1, 5))
            host_second = str(random.randint(1, 5))
            host_rest = ["yes" if random.random() < 0.5 else "no"] + ["yes" if random.random() < 0.6 else "no" for _ in range(3)]
            host_last = str(random.randint(1, 100))
            data.append(f"{i}: host: {host_first}, {host_second}, {', '.join(host_rest)},{host_last}")

    # Append the count of hackers and hosts at the end of the data list
    data.append(f"\nNumber of Hackers: {num_hackers}\nNumber of Hosts: {num_hosts}")

    return data

def write_to_file(data):
    with open("hack-mit.txt", "w") as file:
        for line in data:
            file.write(f"{line}\n")

if __name__ == "__main__":
    generated_data = generate_data()
    write_to_file(generated_data)
    print("Data has been written to 'hack-mit.txt'")
