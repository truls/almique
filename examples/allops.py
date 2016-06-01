from sme import Network, Function, External, Bus, SME

class Producer(Function):
    def setup(self, ins, outs):
        self.map_outs(outs, "out")
        self.v1 = 0
        self.v2 = 0

    def run(self):
        self.out["val1"] = self.v1
        self.out["val2"] = self.v2
        self.v1 += 1
        self.v2 += 1
        if self.v1 > 100:
            self.v1 = 0
            self.v2 = 0

class Add(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, "valbus")
        self.map_outs(outs, "addbus")

    def run(self):
        self.addbus["res"] = self.valbus["val1"] + self.valbus["val2"]


class Sub(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, "valbus")
        self.map_outs(outs, "subbus")

    def run(self):
        self.subbus["res"] = self.valbus["val1"] - self.valbus["val2"]


class Mul(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, "valbus")
        self.map_outs(outs, "mulbus")

    def run(self):
        self.mulbus["res"] = self.valbus["val1"] * self.valbus["val2"]


class Printer(External):
    def setup(self, ins, outs):
        self.map_ins(ins, "addbus", "subbus", "mulbus")

    def run(self):
        print(self.addbus["res"], self.subbus["res"], self.mulbus["res"])

class AllOps(Network):
    def wire(self):
        valbus = Bus("ValueBus", ["val1", "val2"], int)
        valbus["val1"] = 0
        valbus["val2"] = 0
        self.tell(valbus)

        addbus = Bus("AddBus", ["res"], int)
        addbus["res"] = 0
        self.tell(addbus)
        subbus = Bus("SubBus", ["res"], int)
        subbus["res"] = 0
        self.tell(subbus)
        mulbus = Bus("MulBus", ["res"], int)
        mulbus["res"] = 0
        self.tell(mulbus)

        prod = Producer("Producer", [], [valbus])
        self.tell(prod)

        add = Add("Add", [valbus], [addbus])
        self.tell(add)

        sub = Sub("Sub", [valbus], [subbus])
        self.tell(sub)

        mul = Mul("Mul", [valbus], [mulbus])
        self.tell(mul)

        printer = Printer("Printer", [addbus, subbus, mulbus], [])
        self.tell(printer)

def main():
    sme = SME()
    sme.network = AllOps("AllOps")
    sme.network.clock(200)

if __name__ == "__main__":
    main()
