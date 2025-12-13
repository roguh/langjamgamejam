export const pickAnim = (entity, animName = "idle") => {
  if (entity.isJumping()) animName = "jump";
  if (entity.hasAnim(`${animName}_roll`) && entity.isRolling)
    animName = `${animName}_roll`;
  if (entity.curAnim() != animName) entity.play(animName);
};
