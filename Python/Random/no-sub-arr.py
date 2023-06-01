def minSumOfLengths(arr: list, target: int) -> int:
    found = set()

    sum = 0
    curr_range = [0, 0]

    for i, val in enumerate(arr):
        sum += val
        curr_range[1] = i
        if sum > target:
            sum -= arr[curr_range[0]]
            curr_range[0] += 1
        if sum == target:
            found.add(tuple(curr_range))
            sum -= arr[curr_range[0]]
            curr_range[0] += 1
    
    if len(found) < 2:
        print(found)
        return -1
    else:
        print(found)
        
        temp = list()
        for _, val in enumerate(found):
            temp.append(val[1] - val[0] + 1)
        temp.sort()
        return temp[0] + temp[1]

def check_range(found, c_range):
    for val in found:
        if c_range[0] <= val[1] or c_range[1] >= val[0]:
            return False
    return True


if __name__ == '__main__':
    arr = [1,6,1]
    target = 7

    print(minSumOfLengths(arr, target))