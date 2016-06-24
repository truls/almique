from sme import Network, Function, External, Bus, SME, Types
t = Types()


class Gen(Function):
    def setup(self, ins, outs, n):
        self.map_outs(outs, "out")
        self.n = n  # type: t.u3

    def run(self):
        self.out["val"] = self.n


class AddN(Function):
    def setup(self, ins, outs, n):
        self.map_ins(ins, "num")
        self.map_outs(outs, "res")
        self.n = n  # type: t.u
        self.c = 4  # type: t.u2
        self.accum = 0  # type: t.u10

    def run(self):
        self.accum += self.n + self.c + self.num["val"]
        self.res["val"] = self.accum


class Printer(External):
    def setup(self, ins, outs):
        self.map_ins(ins, "res")

    def run(self):
        print(self.res["val"])


class AddNNet(Network):
    def wire(self):
        bus1 = Bus("ValueBus", [t.u2("val")])
        bus1["val"] = 0
        self.tell(bus1)

        bus2 = Bus("InputBus", [t.u10("val")])
        bus2["val"] = 0
        self.tell(bus2)

        gen_param = 2
        gen = Gen("Gen", [], [bus1], gen_param)
        self.tell(gen)

        addn_param = 4
        addn = AddN("AddN", [bus1], [bus2], addn_param)
        self.tell(addn)

        p = Printer("Printer", [bus2], [])
        self.tell(p)


def main():
    sme = SME()
    sme.network = AddNNet("AddNet")
    sme.network.clock(100)

if __name__ == "__main__":
    main()
