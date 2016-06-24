from sme import Network, Function, External, Bus, SME, Types
t = Types()

class Producer(Function):
    def setup(self, ins, outs):
        self.map_outs(outs, "out")
        self.v1 = 0  # type: t.u7
        self.v2 = 0  # type: t.u7

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

class Mul(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, "valbus")
        self.map_outs(outs, "mulbus")

    def run(self):
        self.mulbus["res"] = self.valbus["val1"] * self.valbus["val2"]


class Printer(External):
    def setup(self, ins, outs):
        self.map_ins(ins, "addbus", "mulbus")

    def run(self):
        print(self.addbus["res"], self.subbus["res"], self.mulbus["res"])

class SomeOps(Network):
    def wire(self):
        valbus = Bus("ValueBus", [t.u7("val1"), t.u7("val2")])
        valbus["val1"] = 0
        valbus["val2"] = 0
        self.tell(valbus)

        addbus = Bus("AddBus", [t.u8("res")])
        addbus["res"] = 0
        self.tell(addbus)

        mulbus = Bus("MulBus", [t.u14("res")])
        mulbus["res"] = 0
        self.tell(mulbus)

        prod = Producer("Producer", [], [valbus])
        self.tell(prod)

        add = Add("Add", [valbus], [addbus])
        self.tell(add)

        mul = Mul("Mul", [valbus], [mulbus])
        self.tell(mul)

        printer = Printer("Printer", [addbus, mulbus], [])
        self.tell(printer)

def main():
    sme = SME()
    sme.network = SomeOps("SomeOps")
    sme.network.clock(200)

if __name__ == "__main__":
    main()
