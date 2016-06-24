from sme import SME, Bus, Network, Function


class Prefix(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, 'input')
        self.map_outs(outs, 'output')

    def run(self):
        self.output['val'] = self.input['val']


class Succ(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, 'input')
        self.map_outs(outs, 'output')

    def run(self):
        self.output['val'] = self.input['val'] + 1


class Sink(Function):
    def setup(self, ins, outs):
        self.map_ins(ins, 'inb')

    def run(self):
        #pass
        print(self.inb['val'])


class Commstime(Network):
    def wire(self):
        busa = Bus('BusA', ['val'])
        busa['val'] = 0
        self.tell(busa)
        busb = Bus('BusB', ['val'])
        busb['val'] = 0
        self.tell(busb)

        prefix = Prefix('Prefix', [busb], [busa])
        self.tell(prefix)
        succ = Succ('Succ', [busa], [busb])
        self.tell(succ)
        sink = Sink('Sink', [busa], [])
        self.tell(sink)

# Old version
# v---------(c)--------------------
# prefix -(a)-> delta2 -(b)->succ-|
#                      |
#                     (b)
#                      v
#                     sink
# New version
# v---------(c)------
# prefix -(a)->succ-|
#          |
#         (a)
#          v
#         sink


def main():
    sme = SME()
    sme.network = Commstime("Commstime")

    import time

    N = 1000000
    for _ in range(3):
        t1 = time.time()
        sme.network.clock(N)
        t2 = time.time()

        dt = t2-t1
        cycle = dt / N
        #chan = dt / (3 * N)
        #rint("DT = %f.\nTime per ch : %f/(3*%d) = %f s = %f us" %
        #     (dt, dt, N, tchan, tchan * 1000000))
        print("DT = %f.\nTime per cycle: %f/(3*%d) = %f s = %f us" %
              (dt, dt, N, cycle, cycle * 1000000))
        print("Cycles per second:", N/dt)

        print('----------------------')

if __name__ == "__main__":
    main()
