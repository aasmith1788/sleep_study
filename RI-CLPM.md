Yamada et al. (2022) studied how pain and fatigue influence each other over time in patients undergoing rehabilitation after knee surgery. They used weekly self-reports of pain and fatigue across three time points (weeks 0, 5, and 10) to analyze how changes in one might predict changes in the other. Their main finding was that pain strongly predicted later fatigue, but fatigue did not predict later pain.


three time points work because when trends are linear and consistent across people, like everyone improving at a steady rate, the person’s mean across those three points falls right in the center of the linear trend. That means the random intercept in the RI-CLPM absorbs the trend almost perfectly. Once that intercept is removed from the observed scores, what’s left are residuals that no longer contain the overall increase or decrease. They’re just deviations above or below each person’s own linear path.

With more than three points, that no longer holds. A participant’s mean might no longer align with the middle of the trend if the slope bends slightly, if the trajectory isn’t perfectly linear, or if the variance isn’t spread symmetrically. So the random intercept can’t fully absorb the trend, and the residuals still contain some of it. That contaminates the cross-lagged estimates.

But with three points and smooth linear change, the mean is the midpoint. So subtracting that mean effectively removes the trend. That’s why three waves worked in Yamada’s case and let the RI-CLPM isolate short-term dynamics without modeling the long-term improvement.

<img width="751" height="359" alt="image" src="https://github.com/user-attachments/assets/260ef61d-32f4-4abf-86a4-b64e6f469229" />
<img width="751" height="359" alt="image" src="https://github.com/user-attachments/assets/6a3a47d4-ee87-4142-b817-37be6f35b4af" />
<img width="751" height="359" alt="image" src="https://github.com/user-attachments/assets/1a005694-98f0-41f9-9f31-a87608ab1725" />
<img width="751" height="359" alt="image" src="https://github.com/user-attachments/assets/d302e860-3a1c-4341-8a16-bd4c65cfb34e" />
<img width="751" height="359" alt="image" src="https://github.com/user-attachments/assets/130fd4d5-e6d1-4722-a171-d9017923a352" />
<img width="751" height="359" alt="image" src="https://github.com/user-attachments/assets/2893153a-b18b-4f1f-b59d-16e743468cba" />






