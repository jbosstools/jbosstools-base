package org.jboss.tools.ui.bot.ext.helper;

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.KeyEvent;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.utils.SWTBotPreferences;
import org.eclipse.swtbot.swt.finder.utils.SWTUtils;
import org.jboss.tools.ui.bot.ext.Timing;

public class KeyboardHelper {
  private static Robot robot = null;
  /**
   * Simulate pressing of key with keyCode via SWT
   * @param display
   * @param keyCode
   */
  public static void pressKeyCode (Display display , int keyCode){
    
    Event keyPressed = new Event();
    keyPressed.keyCode = keyCode;   
    keyPressed.type = SWT.KeyDown;
    display.post(keyPressed);
    Event keyReleased = new Event();
      keyReleased.keyCode = keyCode;   
      keyReleased.type = SWT.KeyUp;
    display.post(keyReleased);
    
  }
  /**
   * Simulate pressing of keys with keyCodes via SWT
   * @param display
   * @param keyCodes
   */
  public static void pressKeyCodes (Display display , byte[] keyCodes){
    for (byte keyCode : keyCodes){
      KeyboardHelper.pressKeyCode(display,keyCode);
      SWTUtils.sleep(Timing.time1S());
    }
  }
  /**
   * Simulate pressing of key with keyCode via AWT
   * @param awtKeyCode
   */
  public static void pressKeyCodeUsingAWT (int awtKeyCode){
    try {
      if (KeyboardHelper.robot == null){
        SWTBotPreferences.KEYBOARD_STRATEGY = "org.eclipse.swtbot.swt.finder.keyboard.AWTKeyboardStrategy";
        KeyboardHelper.robot = new Robot();
      }
      KeyboardHelper.robot.keyPress(awtKeyCode);
    } catch (AWTException e) {
      throw new RuntimeException(e);
    }
  }
  /**
   * Simulate releasing of key with keyCode via AWT
   * @param awtKeyCode
   */
  public static void releaseKeyCodeUsingAWT (int awtKeyCode){
    try {
      if (KeyboardHelper.robot == null){
        SWTBotPreferences.KEYBOARD_STRATEGY = "org.eclipse.swtbot.swt.finder.keyboard.AWTKeyboardStrategy";
        KeyboardHelper.robot = new Robot();
      }
      KeyboardHelper.robot.keyRelease(awtKeyCode);
    } catch (AWTException e) {
      throw new RuntimeException(e);
    }
  }
  /**
   * Simulate typing of key with keyCode via AWT
   * @param awtKeyCode
   * @param modifiers keyboard modifiers such CTRL, ALT, SHIFT ...
   */
  public static void typeKeyCodeUsingAWT (int awtKeyCode , int... modifiers){
    if (modifiers != null){
      for (int modifierCode : modifiers){
        KeyboardHelper.pressKeyCodeUsingAWT(modifierCode);
      }
    }
    KeyboardHelper.pressKeyCodeUsingAWT(awtKeyCode);
    KeyboardHelper.releaseKeyCodeUsingAWT(awtKeyCode);
    if (modifiers != null){
      for (int modifierCode : modifiers){
        KeyboardHelper.releaseKeyCodeUsingAWT(modifierCode);
      }
    }
  }
  /**
   * Simulate typing of basic string via AWT
   * @param textkeyCodes - string which can contain only basic characters 0..9, A..Z, a..z
   */
  public static void typeBasicStringUsingAWT (String textKeyCodes){
    for (int index = 0 ; index < textKeyCodes.length() ; index++){
      char ch = textKeyCodes.charAt(index);
      int keyCode = getAWTKeyCode (ch);
      boolean pressShift = (ch >= 'A' && ch <= 'Z');
      if (pressShift){
        KeyboardHelper.pressKeyCodeUsingAWT(KeyEvent.VK_SHIFT);
      }
      KeyboardHelper.typeKeyCodeUsingAWT(keyCode);
      if (pressShift){
        KeyboardHelper.releaseKeyCodeUsingAWT(KeyEvent.VK_SHIFT);
      }
      KeyboardHelper.robot.delay(Timing.time1S());
    }
  }
  /**
   * Gets AWT Key Code for specified character ch
   * @param ch
   * @return
   */
  public static int getAWTKeyCode (char ch){
    int result = KeyEvent.VK_UNDEFINED;
    try {
      result = ReflectionsHelper.getPrivateFieldValue(KeyEvent.class,
        "VK_" + String.valueOf(ch).toUpperCase(),
        null,
        Integer.class);
    } catch (SecurityException e) {
    } catch (IllegalArgumentException e) {
    } catch (NoSuchFieldException e) {
    } catch (IllegalAccessException e) {
    }
    return result;
  }
  /**
   * Selects text from current cursor position and selectionLength length. Selection direction is specified
   * by forward parameter
   * @param forward
   * @param selectionLength
   */
  public static void selectTextUsingAWTKeyBoard (boolean forward , int selectionLength) {
    int arrowCode = forward ? KeyEvent.VK_RIGHT : KeyEvent.VK_LEFT;
    for (int index = 0 ; index < selectionLength; index ++){
      typeKeyCodeUsingAWT(arrowCode,KeyEvent.VK_SHIFT);
    }
  }
  /***
   * Simulate typing of key with keyCode via AWT repeating numOfRepeats times
   * @param awtKeyCode
   * @param numOfRepeats
   * @param modifiers
   */
  public static void typeKeyCodeUsingAWTRepeately (int awtKeyCode , int numOfRepeats, int... modifiers) {
    for (int index = 0 ; index < numOfRepeats ; index++){
      KeyboardHelper.typeKeyCodeUsingAWT(awtKeyCode, modifiers);
    }
  }
}
