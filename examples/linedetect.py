from sme import Network, Function, External, Bus, SME, Types
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.pylab as pylab

t = Types()


def nop(*args, **kwargs):
    pass

debug = nop
# debug = print


class Reader(External):
    def setup(self, ins, outs, results):
        self.map_ins(ins, "controlbus", "databus")
        self.results = results
        self.cnt = 0

    def run(self):
        if self.controlbus["data"] == 1:
            self.results[self.cnt] = self.databus["data"]
            # debug(self.results[self.cnt])
            self.cnt += 1
            if self.cnt == len(self.results):
                self.cnt = 0


class Source(External):
    def setup(self, ins, outs, simdata):
        self.map_ins(ins, "controlbus")
        self.map_outs(outs, "indatabus")
        self.simdata = simdata
        self.simdataidx = 0
        self.phase_incr = 0

    def run(self):
        if self.controlbus["readout"] == True:
            self.indatabus["valid"] = True
            if self.phase_incr == 0:
                self.simdataidx += 1
                self.phase_incr = 1
            debug("Sending data", self.controlbus["selector"], self.simdataidx - 1, self.simdata[self.controlbus["selector"], self.simdataidx - 1])
            self.indatabus["data"] = self.simdata[self.controlbus["selector"], self.simdataidx - 1]

        else:
            self.indatabus["valid"] = False
            self.phase_incr = 0


class Controller(Function):
    def setup(self, ins, outs, rate, pixels):
        self.map_outs(outs, "controlbus")

        self.samplecnt = 0  # type: t.u8
        self.readcnt = 0  # type: t.u8
        self.readoutcnt = 0  # type: t.u8

        self.rate = rate
        self.pixels = pixels

    def run(self):
        # debug("Rate samplecnt, rate is, pixels", self.samplecnt, self.rate, self.pixels)
        if self.samplecnt == self.rate:
            if self.readoutcnt == self.pixels:
                self.readcnt = 1
                self.samplecnt = 1
                self.controlbus["readout"] = False
                self.controlbus["data"] = 0
                self.controlbus['selector'] = self.readoutcnt - 1
                self.readoutcnt = 0
            else:
                if self.readoutcnt == 0:
                    self.controlbus["selector"] = 0
                    self.readoutcnt += 1
                else:
                    self.controlbus["selector"] = self.readoutcnt - 1
                    self.readoutcnt += 1
                self.controlbus["readout"] = True
                self.controlbus["data"] = 0

        else:
            self.controlbus["readout"] = False
            if self.readcnt < self.pixels + 1:
                self.controlbus["selector"] = self.readcnt
                self.controlbus["data"] = 1
            else:
                self.controlbus["selector"] = self.readcnt + 1
                self.readcnt = self.pixels
                self.controlbus["data"] = 0
            self.samplecnt += 1
            self.readcnt += 1



class Pixel(Function):
    def setup(self, ins, outs, id):
        self.map_ins(ins, "controlbus", "indatabus")
        self.map_outs(outs, "databus")
        self.data = 0  # type: t.u8
        self.cnt = 0  # type: t.u8

        self.id = id

    def run(self):
        # debug(self.controlbus["selector"])
        if self.controlbus['readout'] == True and self.controlbus["selector"] == self.id and self.indatabus["valid"] == True:
            self.data = self.indatabus["data"]
            # debug("Id", self.id, "Reading out data", self.data)
            self.cnt += 1
        if self.controlbus['selector'] == self.id:
            # debug("Writing data", self.data)
            self.databus['data'] = self.data
        #else:
        #    self.databus['data'] = 


class System(Network):
    def wire(self, pixels, buffer, rate, simdata, result):
        controlbus = Bus("Control", [t.b('readout'),
                                     t.u8('selector'),
                                     t.u8('data')])
        controlbus['readout'] = False
        controlbus['selector'] = 0
        controlbus['data'] = 0
        self.tell(controlbus)
        databus = Bus('DataBus', [t.u8('data')])
        self.tell(databus)

        indatabus = Bus("InDataBus", [t.b("valid"), t.u8("data")])
        indatabus["valid"] = False
        self.tell(indatabus)

        pixels = 151
        rate = 200
        controller = Controller('Controller', [], [controlbus], rate, pixels)
        self.tell(controller)

        reader = Reader("Reader", [controlbus, databus], [], result)
        self.tell(reader)

        simdata = np.fromfile('plank.dat', dtype=np.uint8)
        simdata.shape = (151, 1080)

        source = Source("Source", [controlbus], [indatabus], simdata)
        self.tell(source)

        num = 0
        pixel0 = Pixel("Pixel0", [controlbus, indatabus], [databus], num)
        self.tell(pixel0)
        num = 1
        pixel1 = Pixel("Pixel1", [controlbus, indatabus], [databus], num)
        self.tell(pixel1)
        num = 2
        pixel2 = Pixel("Pixel2", [controlbus, indatabus], [databus], num)
        self.tell(pixel2)
        num = 3
        pixel3 = Pixel("Pixel3", [controlbus, indatabus], [databus], num)
        self.tell(pixel3)
        num = 4
        pixel4 = Pixel("Pixel4", [controlbus, indatabus], [databus], num)
        self.tell(pixel4)
        num = 5
        pixel5 = Pixel("Pixel5", [controlbus, indatabus], [databus], num)
        self.tell(pixel5)
        num = 6
        pixel6 = Pixel("Pixel6", [controlbus, indatabus], [databus], num)
        self.tell(pixel6)
        num = 7
        pixel7 = Pixel("Pixel7", [controlbus, indatabus], [databus], num)
        self.tell(pixel7)
        num = 8
        pixel8 = Pixel("Pixel8", [controlbus, indatabus], [databus], num)
        self.tell(pixel8)
        num = 9
        pixel9 = Pixel("Pixel9", [controlbus, indatabus], [databus], num)
        self.tell(pixel9)
        num = 10
        pixel10 = Pixel("Pixel10", [controlbus, indatabus], [databus], num)
        self.tell(pixel10)
        num = 11
        pixel11 = Pixel("Pixel11", [controlbus, indatabus], [databus], num)
        self.tell(pixel11)
        num = 12
        pixel12 = Pixel("Pixel12", [controlbus, indatabus], [databus], num)
        self.tell(pixel12)
        num = 13
        pixel13 = Pixel("Pixel13", [controlbus, indatabus], [databus], num)
        self.tell(pixel13)
        num = 14
        pixel14 = Pixel("Pixel14", [controlbus, indatabus], [databus], num)
        self.tell(pixel14)
        num = 15
        pixel15 = Pixel("Pixel15", [controlbus, indatabus], [databus], num)
        self.tell(pixel15)
        num = 16
        pixel16 = Pixel("Pixel16", [controlbus, indatabus], [databus], num)
        self.tell(pixel16)
        num = 17
        pixel17 = Pixel("Pixel17", [controlbus, indatabus], [databus], num)
        self.tell(pixel17)
        num = 18
        pixel18 = Pixel("Pixel18", [controlbus, indatabus], [databus], num)
        self.tell(pixel18)
        num = 19
        pixel19 = Pixel("Pixel19", [controlbus, indatabus], [databus], num)
        self.tell(pixel19)
        num = 20
        pixel20 = Pixel("Pixel20", [controlbus, indatabus], [databus], num)
        self.tell(pixel20)
        num = 21
        pixel21 = Pixel("Pixel21", [controlbus, indatabus], [databus], num)
        self.tell(pixel21)
        num = 22
        pixel22 = Pixel("Pixel22", [controlbus, indatabus], [databus], num)
        self.tell(pixel22)
        num = 23
        pixel23 = Pixel("Pixel23", [controlbus, indatabus], [databus], num)
        self.tell(pixel23)
        num = 24
        pixel24 = Pixel("Pixel24", [controlbus, indatabus], [databus], num)
        self.tell(pixel24)
        num = 25
        pixel25 = Pixel("Pixel25", [controlbus, indatabus], [databus], num)
        self.tell(pixel25)
        num = 26
        pixel26 = Pixel("Pixel26", [controlbus, indatabus], [databus], num)
        self.tell(pixel26)
        num = 27
        pixel27 = Pixel("Pixel27", [controlbus, indatabus], [databus], num)
        self.tell(pixel27)
        num = 28
        pixel28 = Pixel("Pixel28", [controlbus, indatabus], [databus], num)
        self.tell(pixel28)
        num = 29
        pixel29 = Pixel("Pixel29", [controlbus, indatabus], [databus], num)
        self.tell(pixel29)
        num = 30
        pixel30 = Pixel("Pixel30", [controlbus, indatabus], [databus], num)
        self.tell(pixel30)
        num = 31
        pixel31 = Pixel("Pixel31", [controlbus, indatabus], [databus], num)
        self.tell(pixel31)
        num = 32
        pixel32 = Pixel("Pixel32", [controlbus, indatabus], [databus], num)
        self.tell(pixel32)
        num = 33
        pixel33 = Pixel("Pixel33", [controlbus, indatabus], [databus], num)
        self.tell(pixel33)
        num = 34
        pixel34 = Pixel("Pixel34", [controlbus, indatabus], [databus], num)
        self.tell(pixel34)
        num = 35
        pixel35 = Pixel("Pixel35", [controlbus, indatabus], [databus], num)
        self.tell(pixel35)
        num = 36
        pixel36 = Pixel("Pixel36", [controlbus, indatabus], [databus], num)
        self.tell(pixel36)
        num = 37
        pixel37 = Pixel("Pixel37", [controlbus, indatabus], [databus], num)
        self.tell(pixel37)
        num = 38
        pixel38 = Pixel("Pixel38", [controlbus, indatabus], [databus], num)
        self.tell(pixel38)
        num = 39
        pixel39 = Pixel("Pixel39", [controlbus, indatabus], [databus], num)
        self.tell(pixel39)
        num = 40
        pixel40 = Pixel("Pixel40", [controlbus, indatabus], [databus], num)
        self.tell(pixel40)
        num = 41
        pixel41 = Pixel("Pixel41", [controlbus, indatabus], [databus], num)
        self.tell(pixel41)
        num = 42
        pixel42 = Pixel("Pixel42", [controlbus, indatabus], [databus], num)
        self.tell(pixel42)
        num = 43
        pixel43 = Pixel("Pixel43", [controlbus, indatabus], [databus], num)
        self.tell(pixel43)
        num = 44
        pixel44 = Pixel("Pixel44", [controlbus, indatabus], [databus], num)
        self.tell(pixel44)
        num = 45
        pixel45 = Pixel("Pixel45", [controlbus, indatabus], [databus], num)
        self.tell(pixel45)
        num = 46
        pixel46 = Pixel("Pixel46", [controlbus, indatabus], [databus], num)
        self.tell(pixel46)
        num = 47
        pixel47 = Pixel("Pixel47", [controlbus, indatabus], [databus], num)
        self.tell(pixel47)
        num = 48
        pixel48 = Pixel("Pixel48", [controlbus, indatabus], [databus], num)
        self.tell(pixel48)
        num = 49
        pixel49 = Pixel("Pixel49", [controlbus, indatabus], [databus], num)
        self.tell(pixel49)
        num = 50
        pixel50 = Pixel("Pixel50", [controlbus, indatabus], [databus], num)
        self.tell(pixel50)
        num = 51
        pixel51 = Pixel("Pixel51", [controlbus, indatabus], [databus], num)
        self.tell(pixel51)
        num = 52
        pixel52 = Pixel("Pixel52", [controlbus, indatabus], [databus], num)
        self.tell(pixel52)
        num = 53
        pixel53 = Pixel("Pixel53", [controlbus, indatabus], [databus], num)
        self.tell(pixel53)
        num = 54
        pixel54 = Pixel("Pixel54", [controlbus, indatabus], [databus], num)
        self.tell(pixel54)
        num = 55
        pixel55 = Pixel("Pixel55", [controlbus, indatabus], [databus], num)
        self.tell(pixel55)
        num = 56
        pixel56 = Pixel("Pixel56", [controlbus, indatabus], [databus], num)
        self.tell(pixel56)
        num = 57
        pixel57 = Pixel("Pixel57", [controlbus, indatabus], [databus], num)
        self.tell(pixel57)
        num = 58
        pixel58 = Pixel("Pixel58", [controlbus, indatabus], [databus], num)
        self.tell(pixel58)
        num = 59
        pixel59 = Pixel("Pixel59", [controlbus, indatabus], [databus], num)
        self.tell(pixel59)
        num = 60
        pixel60 = Pixel("Pixel60", [controlbus, indatabus], [databus], num)
        self.tell(pixel60)
        num = 61
        pixel61 = Pixel("Pixel61", [controlbus, indatabus], [databus], num)
        self.tell(pixel61)
        num = 62
        pixel62 = Pixel("Pixel62", [controlbus, indatabus], [databus], num)
        self.tell(pixel62)
        num = 63
        pixel63 = Pixel("Pixel63", [controlbus, indatabus], [databus], num)
        self.tell(pixel63)
        num = 64
        pixel64 = Pixel("Pixel64", [controlbus, indatabus], [databus], num)
        self.tell(pixel64)
        num = 65
        pixel65 = Pixel("Pixel65", [controlbus, indatabus], [databus], num)
        self.tell(pixel65)
        num = 66
        pixel66 = Pixel("Pixel66", [controlbus, indatabus], [databus], num)
        self.tell(pixel66)
        num = 67
        pixel67 = Pixel("Pixel67", [controlbus, indatabus], [databus], num)
        self.tell(pixel67)
        num = 68
        pixel68 = Pixel("Pixel68", [controlbus, indatabus], [databus], num)
        self.tell(pixel68)
        num = 69
        pixel69 = Pixel("Pixel69", [controlbus, indatabus], [databus], num)
        self.tell(pixel69)
        num = 70
        pixel70 = Pixel("Pixel70", [controlbus, indatabus], [databus], num)
        self.tell(pixel70)
        num = 71
        pixel71 = Pixel("Pixel71", [controlbus, indatabus], [databus], num)
        self.tell(pixel71)
        num = 72
        pixel72 = Pixel("Pixel72", [controlbus, indatabus], [databus], num)
        self.tell(pixel72)
        num = 73
        pixel73 = Pixel("Pixel73", [controlbus, indatabus], [databus], num)
        self.tell(pixel73)
        num = 74
        pixel74 = Pixel("Pixel74", [controlbus, indatabus], [databus], num)
        self.tell(pixel74)
        num = 75
        pixel75 = Pixel("Pixel75", [controlbus, indatabus], [databus], num)
        self.tell(pixel75)
        num = 76
        pixel76 = Pixel("Pixel76", [controlbus, indatabus], [databus], num)
        self.tell(pixel76)
        num = 77
        pixel77 = Pixel("Pixel77", [controlbus, indatabus], [databus], num)
        self.tell(pixel77)
        num = 78
        pixel78 = Pixel("Pixel78", [controlbus, indatabus], [databus], num)
        self.tell(pixel78)
        num = 79
        pixel79 = Pixel("Pixel79", [controlbus, indatabus], [databus], num)
        self.tell(pixel79)
        num = 80
        pixel80 = Pixel("Pixel80", [controlbus, indatabus], [databus], num)
        self.tell(pixel80)
        num = 81
        pixel81 = Pixel("Pixel81", [controlbus, indatabus], [databus], num)
        self.tell(pixel81)
        num = 82
        pixel82 = Pixel("Pixel82", [controlbus, indatabus], [databus], num)
        self.tell(pixel82)
        num = 83
        pixel83 = Pixel("Pixel83", [controlbus, indatabus], [databus], num)
        self.tell(pixel83)
        num = 84
        pixel84 = Pixel("Pixel84", [controlbus, indatabus], [databus], num)
        self.tell(pixel84)
        num = 85
        pixel85 = Pixel("Pixel85", [controlbus, indatabus], [databus], num)
        self.tell(pixel85)
        num = 86
        pixel86 = Pixel("Pixel86", [controlbus, indatabus], [databus], num)
        self.tell(pixel86)
        num = 87
        pixel87 = Pixel("Pixel87", [controlbus, indatabus], [databus], num)
        self.tell(pixel87)
        num = 88
        pixel88 = Pixel("Pixel88", [controlbus, indatabus], [databus], num)
        self.tell(pixel88)
        num = 89
        pixel89 = Pixel("Pixel89", [controlbus, indatabus], [databus], num)
        self.tell(pixel89)
        num = 90
        pixel90 = Pixel("Pixel90", [controlbus, indatabus], [databus], num)
        self.tell(pixel90)
        num = 91
        pixel91 = Pixel("Pixel91", [controlbus, indatabus], [databus], num)
        self.tell(pixel91)
        num = 92
        pixel92 = Pixel("Pixel92", [controlbus, indatabus], [databus], num)
        self.tell(pixel92)
        num = 93
        pixel93 = Pixel("Pixel93", [controlbus, indatabus], [databus], num)
        self.tell(pixel93)
        num = 94
        pixel94 = Pixel("Pixel94", [controlbus, indatabus], [databus], num)
        self.tell(pixel94)
        num = 95
        pixel95 = Pixel("Pixel95", [controlbus, indatabus], [databus], num)
        self.tell(pixel95)
        num = 96
        pixel96 = Pixel("Pixel96", [controlbus, indatabus], [databus], num)
        self.tell(pixel96)
        num = 97
        pixel97 = Pixel("Pixel97", [controlbus, indatabus], [databus], num)
        self.tell(pixel97)
        num = 98
        pixel98 = Pixel("Pixel98", [controlbus, indatabus], [databus], num)
        self.tell(pixel98)
        num = 99
        pixel99 = Pixel("Pixel99", [controlbus, indatabus], [databus], num)
        self.tell(pixel99)
        num = 100
        pixel100 = Pixel("Pixel100", [controlbus, indatabus], [databus], num)
        self.tell(pixel100)
        num = 101
        pixel101 = Pixel("Pixel101", [controlbus, indatabus], [databus], num)
        self.tell(pixel101)
        num = 102
        pixel102 = Pixel("Pixel102", [controlbus, indatabus], [databus], num)
        self.tell(pixel102)
        num = 103
        pixel103 = Pixel("Pixel103", [controlbus, indatabus], [databus], num)
        self.tell(pixel103)
        num = 104
        pixel104 = Pixel("Pixel104", [controlbus, indatabus], [databus], num)
        self.tell(pixel104)
        num = 105
        pixel105 = Pixel("Pixel105", [controlbus, indatabus], [databus], num)
        self.tell(pixel105)
        num = 106
        pixel106 = Pixel("Pixel106", [controlbus, indatabus], [databus], num)
        self.tell(pixel106)
        num = 107
        pixel107 = Pixel("Pixel107", [controlbus, indatabus], [databus], num)
        self.tell(pixel107)
        num = 108
        pixel108 = Pixel("Pixel108", [controlbus, indatabus], [databus], num)
        self.tell(pixel108)
        num = 109
        pixel109 = Pixel("Pixel109", [controlbus, indatabus], [databus], num)
        self.tell(pixel109)
        num = 110
        pixel110 = Pixel("Pixel110", [controlbus, indatabus], [databus], num)
        self.tell(pixel110)
        num = 111
        pixel111 = Pixel("Pixel111", [controlbus, indatabus], [databus], num)
        self.tell(pixel111)
        num = 112
        pixel112 = Pixel("Pixel112", [controlbus, indatabus], [databus], num)
        self.tell(pixel112)
        num = 113
        pixel113 = Pixel("Pixel113", [controlbus, indatabus], [databus], num)
        self.tell(pixel113)
        num = 114
        pixel114 = Pixel("Pixel114", [controlbus, indatabus], [databus], num)
        self.tell(pixel114)
        num = 115
        pixel115 = Pixel("Pixel115", [controlbus, indatabus], [databus], num)
        self.tell(pixel115)
        num = 116
        pixel116 = Pixel("Pixel116", [controlbus, indatabus], [databus], num)
        self.tell(pixel116)
        num = 117
        pixel117 = Pixel("Pixel117", [controlbus, indatabus], [databus], num)
        self.tell(pixel117)
        num = 118
        pixel118 = Pixel("Pixel118", [controlbus, indatabus], [databus], num)
        self.tell(pixel118)
        num = 119
        pixel119 = Pixel("Pixel119", [controlbus, indatabus], [databus], num)
        self.tell(pixel119)
        num = 120
        pixel120 = Pixel("Pixel120", [controlbus, indatabus], [databus], num)
        self.tell(pixel120)
        num = 121
        pixel121 = Pixel("Pixel121", [controlbus, indatabus], [databus], num)
        self.tell(pixel121)
        num = 122
        pixel122 = Pixel("Pixel122", [controlbus, indatabus], [databus], num)
        self.tell(pixel122)
        num = 123
        pixel123 = Pixel("Pixel123", [controlbus, indatabus], [databus], num)
        self.tell(pixel123)
        num = 124
        pixel124 = Pixel("Pixel124", [controlbus, indatabus], [databus], num)
        self.tell(pixel124)
        num = 125
        pixel125 = Pixel("Pixel125", [controlbus, indatabus], [databus], num)
        self.tell(pixel125)
        num = 126
        pixel126 = Pixel("Pixel126", [controlbus, indatabus], [databus], num)
        self.tell(pixel126)
        num = 127
        pixel127 = Pixel("Pixel127", [controlbus, indatabus], [databus], num)
        self.tell(pixel127)
        num = 128
        pixel128 = Pixel("Pixel128", [controlbus, indatabus], [databus], num)
        self.tell(pixel128)
        num = 129
        pixel129 = Pixel("Pixel129", [controlbus, indatabus], [databus], num)
        self.tell(pixel129)
        num = 130
        pixel130 = Pixel("Pixel130", [controlbus, indatabus], [databus], num)
        self.tell(pixel130)
        num = 131
        pixel131 = Pixel("Pixel131", [controlbus, indatabus], [databus], num)
        self.tell(pixel131)
        num = 132
        pixel132 = Pixel("Pixel132", [controlbus, indatabus], [databus], num)
        self.tell(pixel132)
        num = 133
        pixel133 = Pixel("Pixel133", [controlbus, indatabus], [databus], num)
        self.tell(pixel133)
        num = 134
        pixel134 = Pixel("Pixel134", [controlbus, indatabus], [databus], num)
        self.tell(pixel134)
        num = 135
        pixel135 = Pixel("Pixel135", [controlbus, indatabus], [databus], num)
        self.tell(pixel135)
        num = 136
        pixel136 = Pixel("Pixel136", [controlbus, indatabus], [databus], num)
        self.tell(pixel136)
        num = 137
        pixel137 = Pixel("Pixel137", [controlbus, indatabus], [databus], num)
        self.tell(pixel137)
        num = 138
        pixel138 = Pixel("Pixel138", [controlbus, indatabus], [databus], num)
        self.tell(pixel138)
        num = 139
        pixel139 = Pixel("Pixel139", [controlbus, indatabus], [databus], num)
        self.tell(pixel139)
        num = 140
        pixel140 = Pixel("Pixel140", [controlbus, indatabus], [databus], num)
        self.tell(pixel140)
        num = 141
        pixel141 = Pixel("Pixel141", [controlbus, indatabus], [databus], num)
        self.tell(pixel141)
        num = 142
        pixel142 = Pixel("Pixel142", [controlbus, indatabus], [databus], num)
        self.tell(pixel142)
        num = 143
        pixel143 = Pixel("Pixel143", [controlbus, indatabus], [databus], num)
        self.tell(pixel143)
        num = 144
        pixel144 = Pixel("Pixel144", [controlbus, indatabus], [databus], num)
        self.tell(pixel144)
        num = 145
        pixel145 = Pixel("Pixel145", [controlbus, indatabus], [databus], num)
        self.tell(pixel145)
        num = 146
        pixel146 = Pixel("Pixel146", [controlbus, indatabus], [databus], num)
        self.tell(pixel146)
        num = 147
        pixel147 = Pixel("Pixel147", [controlbus, indatabus], [databus], num)
        self.tell(pixel147)
        num = 148
        pixel148 = Pixel("Pixel148", [controlbus, indatabus], [databus], num)
        self.tell(pixel148)
        num = 149
        pixel149 = Pixel("Pixel149", [controlbus, indatabus], [databus], num)
        self.tell(pixel149)
        num = 150
        pixel150 = Pixel("Pixel150", [controlbus, indatabus], [databus], num)
        self.tell(pixel150)


def main():
    show_graph = False
    do_run = True
    show_result = True
    show_trace = False

    simdata = np.fromfile('plank.dat', dtype=np.uint8)

    simdata.shape = (151, 1080)

    if show_graph:
        camera = System('Camera', 15, 200, 200, simdata, [])
        debug(camera.plot())

    result = [0]*(151*1080)
    if do_run:
        sme = SME()
        sme.network = System('Camera', 151, 151*1080, 200, simdata, result)
        sme.network.clock(1080*200 + (1080*151))

    if show_result:
        plt.rc("font", size=18)
        result = np.array(result)
        debug(len(result))
        result.shape = (1080, 151)
        pylab.matshow(result.T, fignum=100, cmap=pylab.cm.gray)

    if show_trace:
        debug(camera.trace())

    pylab.show()



if __name__ == "__main__":
    main()
