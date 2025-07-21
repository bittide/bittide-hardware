// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#import "@preview/cetz:0.4.0"

#set page(width: auto, height: auto)

#let center-rect(center, (width, height), offset: (0, 0), ..rest) = {
  cetz.draw.get-ctx(ctx => {
    let ctx = ctx
    let (ctx, a) = cetz.coordinate.resolve(ctx, center)
    let base = cetz.vector.add(a, offset)
    let br = cetz.vector.add(base, (-width / 2, -height / 2))
    let tr = cetz.vector.add(br, (width, height))
    return cetz.draw.rect(br, tr, ..rest)
  })
}

#let draw-until-x(from, until, ..rest) = {
  cetz.draw.get-ctx(ctx => {
    let ctx = ctx
    let (ctx, from0, until0) = cetz.coordinate.resolve(ctx, from, until)
    let from1 = cetz.vector.add(from0, (1, 0))
    let until1 = cetz.vector.add(until0, (0, 1))
    let intersect = cetz.intersection.line-line(
      from0,
      from1,
      until0,
      until1,
      ray: true,
    )
    return cetz.draw.line(from0, intersect, ..rest)
  })
}

#let draw-until-y(from, until, ..rest) = {
  cetz.draw.get-ctx(ctx => {
    let ctx = ctx
    let (ctx, from0, until0) = cetz.coordinate.resolve(ctx, from, until)
    let from1 = cetz.vector.add(from0, (0, 1))
    let until1 = cetz.vector.add(until0, (1, 0))
    let intersect = cetz.intersection.line-line(
      from0,
      from1,
      until0,
      until1,
      ray: true,
    )
    return cetz.draw.line(from0, intersect, ..rest)
  })
}

#let vecadd(ctx, a, b) = {
  let (ctx, a, b) = cetz.coordinate.resolve(ctx, a, b)
  return cetz.vector.add(a, b)
}

#cetz.canvas({
  let sidelen = 1.5

  let writeColours = (red, green, blue)
  let nodeColours = (purple, orange, lime)
  let windowColours = (teal, yellow)
  let nNodes = 4
  let cyclesPerWrite = 3
  let repetitions = 2
  let groupCycles = nNodes * cyclesPerWrite
  let windowCycles = (nNodes - 1) * groupCycles
  let padding = windowCycles - groupCycles + 1
  let activeCycles = repetitions * windowCycles
  let metacycleLength = activeCycles + padding
  let displayMetacycles = 2

  let draw-metacycles-linklevel(name, metacycles, base) = cetz.draw.group(
    name: name,
    {
      for metacycle in array.range(0, metacycles) {
        let metacycleBase = cetz.vector.add(
          base,
          (metacycle * metacycleLength * sidelen / groupCycles, 0),
        )
        for window in array.range(0, repetitions) {
          let windowBase = cetz.vector.add(metacycleBase, (
            window * windowCycles * sidelen / groupCycles,
            0,
          ))
          for link in array.range(0, nNodes - 1) {
            let linkBase = cetz.vector.add(windowBase, (link * sidelen, 0))
            let linkName = (
              "linkRect"
                + "["
                + str(metacycle)
                + ","
                + str(window)
                + ","
                + str(link)
                + "]"
            )
            cetz.draw.rect(
              linkBase,
              cetz.vector.add(linkBase, (sidelen, sidelen)),
              fill: nodeColours.at(link).transparentize(80%),
              stroke: nodeColours.at(link),
              name: linkName,
            )
            cetz.draw.content(
              linkName,
              alignment: center,
              box()[
                #set align(center)
                #text()[$D_(#link)$]

                #text()[Link #link]
              ],
            )
          }
        }
        let activeEnd = cetz.vector.add(metacycleBase, (
          activeCycles * sidelen / cyclesPerWrite / nNodes,
          0,
        ))
        cetz.draw.rect(
          activeEnd,
          cetz.vector.add(activeEnd, (
            padding * sidelen / cyclesPerWrite / nNodes,
            sidelen,
          )),
          stroke: black,
          fill: silver,
          name: ("paddingRect[" + str(metacycle) + "]"),
        )
        cetz.draw.content(
          ("paddingRect[" + str(metacycle) + "]"),
          alignment: center,
          [padding],
        )
      }
    },
  )

  let draw-metacycles-activelevel(name, metacycles, base) = cetz.draw.group(
    name: name,
    {
      for metacycle in array.range(0, metacycles) {
        let metacycleBase = cetz.vector.add(
          base,
          (metacycle * metacycleLength * sidelen / groupCycles, 0),
        )
        let activeEnd = cetz.vector.add(metacycleBase, (
          activeCycles * sidelen / cyclesPerWrite / nNodes,
          0,
        ))
        cetz.draw.rect(
          metacycleBase,
          cetz.vector.add(activeEnd, (0, sidelen)),
          stroke: lime,
          fill: lime.transparentize(80%),
          name: "activeRect[" + str(metacycle) + "]",
        )
        cetz.draw.content(
          ("activeRect[" + str(metacycle) + "]"),
          alignment: center,
          [active],
        )
        cetz.draw.rect(
          activeEnd,
          cetz.vector.add(activeEnd, (
            padding * sidelen / cyclesPerWrite / nNodes,
            sidelen,
          )),
          stroke: black,
          fill: silver,
          name: ("paddingRect[" + str(metacycle) + "]"),
        )
        cetz.draw.content(
          ("paddingRect[" + str(metacycle) + "]"),
          alignment: center,
          [padding],
        )
      }
    },
  )

  let base0 = (0, 0)
  draw-metacycles-linklevel("part00", 2, base0)
  cetz.draw.get-ctx(ctx => {
    cetz.draw.line(
      vecadd(ctx, "part00.linkRect[0,0,0].north-west", (0, 0.5)),
      vecadd(ctx, "part00.linkRect[0,0,2].north-west", (0, 0.5)),
      mark: (end: (symbol: "stealth", fill: black)),
      name: "o0",
    )
    cetz.draw.content("o0", box(fill: white, inset: 1pt, [o]))
  })
  center-rect(
    "part00.west",
    (3, 3),
    offset: (-4, 0),
    stroke: yellow,
    fill: yellow.transparentize(80%),
    name: "read0",
  )
  draw-metacycles-activelevel("part01", 2, cetz.vector.add(base0, (0.5, -4)))
  center-rect(
    "read0.center",
    (3, 3),
    offset: (0, -4),
    stroke: eastern,
    fill: eastern.transparentize(80%),
    name: "write0",
  )
  draw-until-x(
    ("read0.south", 0.5, "write0.north"),
    (
      "part00.paddingRect[1].south-east",
      0.5,
      "part01.paddingRect[1].north-east",
    ),
    stroke: (dash: "dashed"),
    name: "cdc0",
  )
  cetz.draw.content("cdc0", box(fill: white, inset: 1pt, [clock domain crossing]))
  cetz.draw.get-ctx(ctx => {
    let base = vecadd(ctx, ("read0.south", 0.6, "write0.north"), (-3, 0))
    cetz.draw.content(
      base,
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(24pt)
        Case 1: Writing metacycle starts first
      ],
    )
  })
  draw-until-y(
    "part01.activeRect[0].north-west",
    "part00.linkRect[0,0,0].south",
    mark: (end: (symbol: "stealth", fill: black))
  )
  cetz.draw.get-ctx(ctx => {
    let base = vecadd(
      ctx,
      "part00.linkRect[0,0,2].south-west",
      (0, -0.5),
    )
    draw-until-x(
      base,
      "part01.activeRect[0].north-west",
      mark: (start: (symbol: "stealth", fill: black)),
      name: "k0",
    )
    cetz.draw.content("k0", box(fill: white, inset: 1pt, [k]))
  })

  let base1 = (0, -10)
  draw-metacycles-linklevel("part10", 2, base1)
  cetz.draw.get-ctx(ctx => {
    cetz.draw.line(
      vecadd(ctx, "part10.linkRect[0,0,0].north-west", (0, 0.5)),
      vecadd(ctx, "part10.linkRect[0,1,2].north-west", (0, 0.5)),
      mark: (end: (symbol: "stealth", fill: black)),
      name: "o1",
    )
    cetz.draw.content("o1", box(fill: white, inset: 1pt, [o]))
  })
  center-rect(
    "part10.west",
    (3, 3),
    offset: (-4, 0),
    stroke: yellow,
    fill: yellow.transparentize(80%),
    name: "read1",
  )
  draw-metacycles-activelevel("part11", 2, cetz.vector.add(base1, (3.6, -4)))
  center-rect(
    "read1.center",
    (3, 3),
    offset: (0, -4),
    stroke: eastern,
    fill: eastern.transparentize(80%),
    name: "write1",
  )
  draw-until-x(
    ("read1.south", 0.5, "write1.north"),
    (
      "part10.paddingRect[1].south-east",
      0.5,
      "part11.paddingRect[1].north-east",
    ),
    stroke: (dash: "dashed"),
    name: "cdc1",
  )
  cetz.draw.content("cdc1", box(fill: white, inset: 1pt, [clock domain crossing]))
  cetz.draw.get-ctx(ctx => {
    let base = vecadd(ctx, ("read1.south", 0.5, "write1.north"), (-3, 0))
    cetz.draw.content(
      base,
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(24pt)
        #set align(right)
        Case 2: Read metacycle starts first,

        but write may occur in same metacycle
      ],
    )
  })
  draw-until-y(
    "part11.activeRect[0].north-west",
    "part10.linkRect[0,0,0].south",
    mark: (end: (symbol: "stealth", fill: black))
  )
  cetz.draw.get-ctx(ctx => {
    let base = vecadd(
      ctx,
      "part10.linkRect[0,1,2].south-west",
      (0, -0.5),
    )
    draw-until-x(
      base,
      "part11.activeRect[0].north-west",
      mark: (start: (symbol: "stealth", fill: black)),
      name: "k1",
    )
    cetz.draw.content("k1", box(fill: white, inset: 1pt, [k]))
  })

  let base2 = (0, -20)
  draw-metacycles-linklevel("part20", 2, base2)
  cetz.draw.get-ctx(ctx => {
    cetz.draw.line(
      vecadd(ctx, "part20.linkRect[1,0,0].north-west", (0, 0.5)),
      vecadd(ctx, "part20.linkRect[1,0,2].north-west", (0, 0.5)),
      mark: (end: (symbol: "stealth", fill: black)),
      name: "o2",
    )
    cetz.draw.content("o2", box(fill: white, inset: 1pt, [o]))
  })
  center-rect(
    "part20.west",
    (3, 3),
    offset: (-4, 0),
    stroke: yellow,
    fill: yellow.transparentize(80%),
    name: "read2",
  )
  draw-metacycles-activelevel("part21", 1, cetz.vector.add(base2, (8.4, -4)))
  center-rect(
    "read2.center",
    (3, 3),
    offset: (0, -4),
    stroke: eastern,
    fill: eastern.transparentize(80%),
    name: "write2",
  )
  draw-until-x(
    ("read2.south", 0.5, "write2.north"),
    (
      "part20.paddingRect[1].south-east",
      0.5,
      "part21.paddingRect[0].north-east",
    ),
    stroke: (dash: "dashed"),
    name: "cdc2",
  )
  cetz.draw.get-ctx(ctx => {
    let base = vecadd(ctx, ("read2.south", 0.5, "write2.north"), (-3, 0))
    cetz.draw.content(
      base,
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(24pt)
        #set align(right)
        Case 3: Read metacycle starts first,

        but write must occur in next metacycle
      ],
    )
  })
  draw-until-y(
    "part21.activeRect[0].north-west",
    "part20.linkRect[0,0,0].south",
    mark: (end: (symbol: "stealth", fill: black))
  )
  cetz.draw.get-ctx(ctx => {
    let base = vecadd(
      ctx,
      "part20.linkRect[1,0,2].south-west",
      (0, -0.5),
    )
    draw-until-x(
      base,
      "part21.activeRect[0].north-west",
      mark: (start: (symbol: "stealth", fill: black)),
      name: "k2",
    )
    cetz.draw.content("k2", box(fill: white, inset: 1pt, [k]))
  })
  cetz.draw.content("cdc2", box(fill: white, inset: 1pt, [clock domain crossing]))
})
