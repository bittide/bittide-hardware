// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#import "@preview/cetz:0.4.0"

#set page(width: auto, height: auto)

#cetz.canvas({
  import cetz.draw: *

  let sidelen = 1.5

  let writeColours = (red, green, blue)
  let nodeColours = (purple, lime, orange)
  let windowColours = (teal, yellow)
  let nNodes = 4
  let cyclesPerWrite = 3
  let repetitions = 2
  let groupCycles = nNodes * cyclesPerWrite
  let windowCycles = (nNodes - 1) * groupCycles
  let padding = windowCycles * 2
  let activeCycles = repetitions * windowCycles
  let metacycleLength = activeCycles + padding
  let displayMetacycles = 2

  let base1 = (0, 0)
  group(name: "level0", {
    for window in array.range(0, repetitions) {
      let windowBase = cetz.vector.add(base1, (
        windowCycles * window * sidelen,
        0,
      ))
      for link in array.range(0, nNodes - 1) {
        let linkBase = cetz.vector.add(windowBase, (
          groupCycles * link * sidelen,
          0,
        ))
        for group in array.range(0, nNodes) {
          let groupBase = cetz.vector.add(linkBase, (
            group * cyclesPerWrite * sidelen,
            0,
          ))
          for write in array.range(0, cyclesPerWrite) {
            let writeBase = cetz.vector.add(groupBase, (write * sidelen, 0))
            let rectName = (
              "writeRect["
                + str(window)
                + ","
                + str(link)
                + ","
                + str(group)
                + ","
                + str(write)
                + "]"
            )
            rect(
              writeBase,
              cetz.vector.add(writeBase, (sidelen, sidelen)),
              fill: writeColours.at(write).transparentize(80%),
              stroke: nodeColours.at(link),
              name: rectName,
            )
            content(
              rectName,
              alignment: center,
              box()[
                #set align(center)
                #text()[$D_(#link,#group,#write)$]

                #text()[Link #link]
              ],
            )
          }
        }
      }
    }
    let activeEnd = cetz.vector.add(base1, (activeCycles * sidelen, 0))
    rect(
      activeEnd,
      cetz.vector.add(activeEnd, (padding * sidelen, sidelen)),
      stroke: black,
      fill: silver,
      name: "paddingRect",
    )
    content(
      "paddingRect",
      alignment: center,
      [padding],
    )
  })

  get-ctx(ctx => {
    content(
      cetz.coordinate.resolve-anchor(ctx, "level0.west"),
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(18pt)
        Cycle-level depiction of metacycle:
      ],
    )
  })

  // Compacted diagram 1
  let base2 = (0, -7)
  group(name: "level1", {
    for metacycle in array.range(0, displayMetacycles) {
      let metacycleBase = cetz.vector.add(base2, (
        metacycle * metacycleLength * sidelen / cyclesPerWrite,
        0,
      ))
      for window in array.range(0, repetitions) {
        let windowBase = cetz.vector.add(metacycleBase, (
          window * windowCycles * sidelen / cyclesPerWrite,
          0,
        ))
        for link in array.range(0, nNodes - 1) {
          let linkBase = cetz.vector.add(windowBase, (
            groupCycles * link * sidelen / cyclesPerWrite,
            0,
          ))
          for group in array.range(0, nNodes) {
            let groupBase = cetz.vector.add(linkBase, (sidelen * group, 0))
            let groupName = (
              "groupRect["
                + str(metacycle)
                + ","
                + str(window)
                + ","
                + str(link)
                + ","
                + str(group)
                + "]"
            )
            rect(
              groupBase,
              cetz.vector.add(groupBase, (sidelen, sidelen)),
              fill: nodeColours.at(link).transparentize(80%),
              stroke: nodeColours.at(link),
              name: groupName,
            )
            content(
              groupName,
              alignment: center,
              box()[
                #set align(center)
                #text()[$D_(#link,#group)$]

                #text()[Link #link]
              ],
            )
          }
        }
      }
      let activeEnd = cetz.vector.add(metacycleBase, (
        activeCycles * sidelen / cyclesPerWrite,
        0,
      ))
      rect(
        activeEnd,
        cetz.vector.add(activeEnd, (
          padding * sidelen / cyclesPerWrite,
          sidelen,
        )),
        stroke: black,
        fill: silver,
        name: "paddingRect",
      )
      content(
        "paddingRect",
        alignment: center,
        [padding],
      )
    }
  })

  get-ctx(ctx => {
    let p0 = cetz.coordinate.resolve-anchor(
      ctx,
      "level1.groupRect[0,0,0,0].north-west",
    )
    let p1 = cetz.coordinate.resolve-anchor(
      ctx,
      "level1.groupRect[0,0,0,0].north-east",
    )
    for i in array.range(0, cyclesPerWrite) {
      line(
        "level0.writeRect[0,0,0," + str(i) + "].south",
        cetz.vector.lerp(p0, p1, (i + 1) / (cyclesPerWrite + 1)),
        mark: (end: (symbol: "stealth", fill: black)),
      )
    }

    content(
      cetz.coordinate.resolve-anchor(ctx, "level1.west"),
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(18pt)
        Write-level depiction of metacycle:
      ],
    )
  })

  // Compacted diagram 2
  let base3 = (0, -14)
  group(name: "level2", {
    for metacycle in array.range(0, displayMetacycles) {
      let metacycleBase = cetz.vector.add(base3, (
        metacycle * metacycleLength * sidelen / cyclesPerWrite / nNodes,
        0,
      ))
      for window in array.range(0, repetitions) {
        let windowBase = cetz.vector.add(metacycleBase, (
          window * windowCycles * sidelen / cyclesPerWrite / nNodes,
          0,
        ))
        for link in array.range(0, nNodes - 1) {
          let linkBase = cetz.vector.add(windowBase, (
            groupCycles * link * sidelen / cyclesPerWrite / nNodes,
            0,
          ))
          let linkName = (
            "linkRect["
              + str(metacycle)
              + ","
              + str(window)
              + ","
              + str(link)
              + "]"
          )
          rect(
            linkBase,
            cetz.vector.add(linkBase, (sidelen, sidelen)),
            fill: nodeColours.at(link).transparentize(80%),
            stroke: nodeColours.at(link),
            name: linkName,
          )
          content(
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
      rect(
        activeEnd,
        cetz.vector.add(activeEnd, (
          padding * sidelen / cyclesPerWrite / nNodes,
          sidelen,
        )),
        stroke: black,
        fill: silver,
        name: "paddingRect",
      )
      content(
        "paddingRect",
        alignment: center,
        [padding],
      )
    }
  })

  get-ctx(ctx => {
    let p0 = cetz.coordinate.resolve-anchor(
      ctx,
      "level2.linkRect[0,0,0].north-west",
    )
    let p1 = cetz.coordinate.resolve-anchor(
      ctx,
      "level2.linkRect[0,0,0].north-east",
    )
    for i in array.range(0, nNodes) {
      line(
        "level1.groupRect[0,0,0," + str(i) + "].south",
        cetz.vector.lerp(
          cetz.coordinate.resolve-anchor(
            ctx,
            "level2.linkRect[0,0,0].north-west",
          ),
          cetz.coordinate.resolve-anchor(
            ctx,
            "level2.linkRect[0,0,0].north-east",
          ),
          (i + 1) / (nNodes + 1),
        ),
        mark: (end: (symbol: "stealth", fill: black)),
      )
    }

    content(
      cetz.coordinate.resolve-anchor(ctx, "level2.west"),
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(18pt)
        Group-level depiction of metacycle:
      ],
    )
  })

  // Compacted diagram 3
  let base3 = (0, -21)
  group(name: "level3", {
    for metacycle in array.range(0, displayMetacycles) {
      let metacycleBase = cetz.vector.add(base3, (
        metacycle
          * metacycleLength
          * sidelen
          / cyclesPerWrite
          / nNodes
          / (nNodes - 1),
        0,
      ))
      for window in array.range(0, repetitions) {
        let windowBase = cetz.vector.add(metacycleBase, (
          window
            * windowCycles
            * sidelen
            / cyclesPerWrite
            / nNodes
            / (nNodes - 1),
          0,
        ))
        let windowName = (
          "windowRect[" + str(metacycle) + "," + str(window) + "]"
        )
        rect(
          windowBase,
          cetz.vector.add(windowBase, (sidelen, sidelen)),
          fill: windowColours.at(window).transparentize(80%),
          stroke: windowColours.at(window),
          name: windowName,
        )
        content(
          windowName,
          alignment: center,
          box()[
            #set align(center)
            #text()[$W_(#window)$]
          ],
        )
      }
      let activeEnd = cetz.vector.add(metacycleBase, (
        activeCycles * sidelen / cyclesPerWrite / nNodes / (nNodes - 1),
        0,
      ))
      rect(
        activeEnd,
        cetz.vector.add(activeEnd, (
          padding * sidelen / cyclesPerWrite / nNodes / (nNodes - 1),
          sidelen,
        )),
        stroke: black,
        fill: silver,
        name: "paddingRect",
      )
      content(
        "paddingRect",
        alignment: center,
        [padding],
      )
    }
  })

  get-ctx(ctx => {
    let p0 = cetz.coordinate.resolve-anchor(
      ctx,
      "level3.windowRect[0,0].north-west",
    )
    let p1 = cetz.coordinate.resolve-anchor(
      ctx,
      "level3.windowRect[0,0].north-east",
    )
    for i in array.range(0, nNodes - 1) {
      line(
        "level2.linkRect[0,0," + str(i) + "].south",
        cetz.vector.lerp(p0, p1, (i + 1) / nNodes),
        mark: (end: (symbol: "stealth", fill: black)),
      )
    }

    content(
      cetz.coordinate.resolve-anchor(ctx, "level3.west"),
      anchor: "mid-east",
      padding: (right: 0.5),
      [
        #set text(18pt)
        Window-level depiction of metacycle:
      ],
    )
  })
})
