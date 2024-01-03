import random

def generate_data():
    data = []
    options = ["hacker", "host"]

    # Generate between 15 and 20 lines
    num_lines = random.randint(4, 6)

    for i in range(1, num_lines + 1):
        # Randomly choose between hacker and host based on the specified probabilities
        choice = random.choices(options, weights=[0.6, 0.4], k=1)[0]

        if choice == "hacker":
            # For hacker, generate yes or no randomly
            values = ["yes" if random.random() < 0.5 else "no" for _ in range(6)]
            data.append(f"{i}, hacker: {', '.join(values)}")
        else:
            # For host, first two parameters are between 1 and 5, rest are yes or no randomly
            host_first = str(random.randint(1, 5))
            host_second = str(random.randint(1, 5))
            host_rest = ["yes" if random.random() < 0.5 else "no" for _ in range(4)]
            data.append(f"{i}: host: {host_first}, {host_second}, {', '.join(host_rest)}")

    return data

if __name__ == "__main__":
    data = generate_data()
    for line in data:
        print(line)
