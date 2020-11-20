# jaccard係数
def jaccard(list_a, list_b):
    n_intersection = len(set.intersection(set(list_a), set(list_b)))
    n_union = len(set.union(set(list_a), set(list_b)))

    try:
        return float(n_intersection) / n_union
    except ZeroDivisionError:
        return 1.0


# dice係数
def dice(list_a, list_b):
    n_intersection = len(set.intersection(set(list_a), set(list_b)))
    n_member = len(set(list_a)) + len(set(list_b))

    try:
        return 2.0 * n_intersection / n_member
    except ZeroDivisionError:
        return 1.0


# simpson係数
def simpson(list_a, list_b):
    n_intersection = len(set.intersection(set(list_a), set(list_b)))
    n_member_min = min(len(set(list_a)), len(set(list_b)))

    try:
        return n_intersection / n_member_min
    except ZeroDivisionError:
        return 1.0
