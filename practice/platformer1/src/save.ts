let lastSaveTime = -10;
export const saveTime = (set?: number) => {
  if (set) lastSaveTime = set;
  return lastSaveTime;
};

export const fastSaveGame = (player) => {
  localStorage.setItem(
    "lstr48Save",
    JSON.stringify({
      player: player.toSaveFile(),
    }),
  );
};

export const loadGame = (player) => {
  const savedGame = localStorage.getItem("lstr48Save");
  if (savedGame) {
    const { player: p } = JSON.parse(savedGame);
    player.fromSaveFile(p);
  }
};
