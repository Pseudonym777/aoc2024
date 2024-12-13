
def main():
    m = Map("input.txt")
    print(m.scoreAllP2())

class Map:
    nodes = [[]]
    dims = (0,0)
    def __init__(self,file):
        lines = open(file).read().splitlines()
        self.nodes = [[int(x) for x in line] for line in lines]
        self.dims = (len(self.nodes),len(self.nodes[0]))
    def inbounds(self,x,y):
        return x >= 0 and x < self.dims[0] and y >= 0 and y < self.dims[1]

    def ups(self,x,y):
        close = [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]
        return [(a,b) for (a,b) in close if self.inbounds(a,b) and self.nodes[a][b] == self.nodes[x][y] + 1]
    def scoreOne(self,x,y):
        if self.nodes[x][y] != 0:
            return 0
        tips = {(x,y)}
        for i in range(9):
            tips = set({}).union(*(set(self.ups(*t)) for t in tips))

        return len(tips)
    def scoreOneP2(self,x,y):
        if self.nodes[x][y] != 0:
            return 0
        tips = [(x,y)]
        for i in range(9):
            tips =sum([self.ups(*t) for t in tips],[])
        return len(tips)

    def scoreAll(self):
        return sum(self.scoreOne(x,y) for x in range(self.dims[0]) for y in range(self.dims[1]))
    def scoreAllP2(self):
        return sum(self.scoreOneP2(x,y) for x in range(self.dims[0]) for y in range(self.dims[1]))












main()

