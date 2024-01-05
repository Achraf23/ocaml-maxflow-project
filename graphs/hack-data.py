import random

def generate_data():
    data = []
    options = ["hacker", "host"]

    # Generate between 15 and 20 lines
    num_lines = random.randint(15, 20)

    for i in range(1, num_lines + 1):
        # Randomly choose between hacker and host based on the specified probabilities
        choice = random.choices(options, weights=[0.5, 0.5], k=1)[0]

        if choice == "hacker":
            # For hacker, generate yes or no randomly for each of the 5 parameters plus the new one
            values = ["yes" if random.random() < 0.6 else "no" for _ in range(5)] + ["yes" if random.random() < 0.5 else "no"]
            data.append(f"{i}, hacker: {', '.join(values)}")
        else:
            # For host, first two parameters are between 1 and 5, rest are yes or no randomly, including the new one
            host_first = str(random.randint(1, 5))
            host_second = str(random.randint(1, 5))
            host_rest = ["yes" if random.random() < 0.6 else "no" for _ in range(3)] + ["yes" if random.random() < 0.5 else "no"]
            data.append(f"{i}: host: {host_first}, {host_second}, {', '.join(host_rest)}")

    return data

def write_to_file(data):
    with open("hack-mit.txt", "w") as file:
        for line in data:
            file.write(f"{line}\n")

if __name__ == "__main__":
    generated_data = generate_data()
    write_to_file(generated_data)
    print("Data has been written to 'hack-mit.txt'")
