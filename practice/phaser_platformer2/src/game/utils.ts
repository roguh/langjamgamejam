export const wrap = (str: string, maxLen: number): string[] => {
  // implement wrap logic here
  const words = str.split(" ");
  const lines = [];
  let currentLine = "";

  for (const word of words) {
    if (currentLine.length + word.length + 1 > maxLen) {
      lines.push(currentLine);
      currentLine = word;
    } else {
      currentLine += (currentLine.length > 0 ? " " : "") + word;
    }
  }

  if (currentLine.length > 0) {
    lines.push(currentLine);
  }

  return lines;
};
