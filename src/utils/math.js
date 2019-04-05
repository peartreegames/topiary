export function rnd() {
  let result = ""
  for (let i = 0; i < 8; i++) {
    result += (function alpha() {
      return String.fromCharCode(Math.random() * (122 - 48) + 48).replace(
        // eslint-disable-next-line
        /[\;\:\<\>\?\@\\\`\_\]\[\^\`\=]/,
        () => alpha()
      )
    })()
  }
  return result
}

export function determineEdge(start1, end1, start2, end2) {
  if (!start1 || !end1 || !start2 || !end2) {
    return {}
  }

  const deltaA1 = end1.y - start1.y
  const deltaB1 = start1.x - end1.x
  const deltaC1 = end1.x * start1.y - start1.x * end1.y
  const s4 = deltaA1 * start2.x + deltaB1 * start2.y + deltaC1
  const s3 = deltaA1 * end2.x + deltaB1 * end2.y + deltaC1
  if (s3 !== 0 && s4 !== 0 && ((s3 >= 0 && s4 >= 0) || (s3 < 0 && s4 < 0))) {
    return
  }

  const deltaA2 = end2.y - start2.y
  const deltaB2 = start2.x - end2.x
  const deltaC2 = end2.x * start2.y - start2.x * end2.y
  const s1 = deltaA2 * start1.x + deltaB2 * start1.y + deltaC2
  const s2 = deltaA2 * end1.x + deltaB2 * end1.y + deltaC2
  if (s1 !== 0 && s2 !== 0 && ((s1 >= 0 && s2 >= 0) || (s1 < 0 && s2 < 0))) {
    return
  }

  const denominator = deltaA1 * deltaB2 - deltaA2 * deltaB1
  const x = deltaB1 * deltaC2 - deltaB2 * deltaC1
  const y = deltaA2 * deltaC1 - deltaA1 * deltaC2
  return {
    x: x / denominator,
    y: y / denominator
  }
}
