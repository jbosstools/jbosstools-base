 /*******************************************************************************
  * Copyright (c) 2007-2010 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.helper;

import static org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable.syncExec;

import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.text.MessageFormat;

import org.apache.log4j.Logger;
import org.eclipse.draw2d.FigureCanvas;
import org.eclipse.jface.util.Geometry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
/**
 * Adds DnD Bot functionality
 * @author jlukas, Vladimir Pakan
 *
 */
public class DragAndDropHelper {
  private static final Logger log = Logger.getLogger(DragAndDropHelper.class);
  private static final int robotDelay = Integer.getInteger("org.jboss.tools.ui.bot.ext.dndDelay", 150);
  private static Robot robot = null;
  /**
   * Drag&Drop sourceWidget on to targetWidget
   * @param sourceWidget
   * @param targetWidget
   */
  public static void dragAndDropOnTo(Widget sourceWidget, Widget targetWidget){
    log.info("Drag and Drop from " + sourceWidget + " to " + targetWidget);
    try {
      if (DragAndDropHelper.robot == null){
        DragAndDropHelper.robot = new Robot();
      }
      DragAndDropHelper.dragAndDropOnTo(calculateWidgetPositionOn(sourceWidget),
        calculateWidgetPositionOn(targetWidget));
    } catch (AWTException e) {
      throw new RuntimeException(e);
    }
  }
  
  /**
   * Drag&Drop from sourcePoint on to targetPoint
   * @param sourcePoint
   * @param targetPoint
   */
  private static void dragAndDropOnTo(final Point sourcePoint,final Point targetPoint){
    try {
      if (DragAndDropHelper.robot == null){
        DragAndDropHelper.robot = new Robot();
      }
      // the x+10 motion is needed to let native functions register a drag detect. It did not work under Windows
      // otherwise and has been reported to be required for linux, too. But I could not test that.
      syncExec(new VoidResult() {
        public void run() {
          DragAndDropHelper.robot.mouseMove(sourcePoint.x, sourcePoint.y);
          DragAndDropHelper.robot.mousePress(InputEvent.BUTTON1_MASK);
          DragAndDropHelper.robot.mouseMove((sourcePoint.x + 10), sourcePoint.y);
        }
      });
      // now pause the test until all runnables on the Display thread have run this is necessary for the pick up
      // to register on linux
      DragAndDropHelper.waitForIdle(DragAndDropHelper.robot);
      
      syncExec(new VoidResult() {
        public void run() {
          DragAndDropHelper.robot.mouseMove((targetPoint.x + 10), targetPoint.y);
          DragAndDropHelper.robot.mouseMove(targetPoint.x, targetPoint.y);
        }
      });
      
      DragAndDropHelper.waitForIdle(DragAndDropHelper.robot);
      
      syncExec(new VoidResult() {
        public void run() {
          DragAndDropHelper.robot.mouseRelease(InputEvent.BUTTON1_MASK);
        }
      });
      waitForIdle(DragAndDropHelper.robot);
    } catch (AWTException e) {
      throw new RuntimeException(e);
    }
  }
  /**
   * Needed for linux
   * @param robot
   */
  private static void waitForIdle(final Robot robot) {
    if (SWT.getPlatform().equals("gtk")) {
        robot.waitForIdle();
    }
  }
  /**
   * Calculate position of point above widget to be used as mouse click point for DnD
   * @param widget
   * @return
   */
  private static Point calculateWidgetPositionOn (final Widget widget){
    Point result = new Point(0,0);
    final Rectangle sourceLocation = DragAndDropHelper.absoluteLocation(widget);
    result.x = sourceLocation.x + 10;
    result.y = sourceLocation.y + 10;
    return result;
  }
  /**
   * Gets absolute location of widget
   * @param widget
   * @return
   */
  private static Rectangle absoluteLocation(final Widget widget) {
    return syncExec(new Result<Rectangle>() {
      public Rectangle run() {
        if (widget instanceof TreeItem){
          return Display.getDefault().map(((TreeItem)widget).getParent(),
            null,
            ((TreeItem)widget).getBounds());
        }
        else{
          return Display.getDefault().map(((Control)widget).getParent(), 
            null,
            ((Control)widget).getBounds());  
        }
      }
    });
  }  

  public static void dnd(final TreeItem ti, final TreeItem ti2) {
    Rectangle r1 = UIThreadRunnable.syncExec(new Result<Rectangle>() {

      public Rectangle run() {
        return ti.getDisplay().map(ti.getParent(), null, ti.getBounds());
      }
    });
    final Point slightOffset = Geometry.add(Geometry.getLocation(r1),
        new Point(10, 10));

    Rectangle r2 = UIThreadRunnable.syncExec(new Result<Rectangle>() {

      public Rectangle run() {
        return ti2.getDisplay().map(ti2.getParent(), null, ti2.getBounds());
      }
    });

    doDragAndDrop(Geometry.min(Geometry.centerPoint(r1), slightOffset),
        Geometry.centerPoint(r2));
  }

  public static void dnd(final TreeItem ti, final FigureCanvas fc) {
    Rectangle r1 = UIThreadRunnable.syncExec(new Result<Rectangle>() {

      public Rectangle run() {
        return ti.getDisplay().map(ti.getParent(), null, ti.getBounds());
      }
    });
    final Point slightOffset = Geometry.add(Geometry.getLocation(r1),
        new Point(10, 10));

    Rectangle r2 = UIThreadRunnable.syncExec(new Result<Rectangle>() {

      public Rectangle run() {
        return fc.getDisplay().map(fc.getParent(), null, fc.getBounds());
      }
    });

    doDragAndDrop(Geometry.min(Geometry.centerPoint(r1), slightOffset),
        Geometry.centerPoint(r2));
  }

  public static void dnd(final TreeItem ti, final Control fc, final int x,
      final int y) {
    Rectangle r1 = UIThreadRunnable.syncExec(new Result<Rectangle>() {

      public Rectangle run() {
        return ti.getDisplay().map(ti.getParent(), null, ti.getBounds());
      }
    });
    final Point slightOffset = Geometry.add(Geometry.getLocation(r1),
        new Point(10, 10));

    Point r2 = UIThreadRunnable.syncExec(new Result<Point>() {

      public Point run() {
        log.info("xxx: " + fc.getLocation().x + ":" + fc.getLocation().y);
        return fc.getDisplay().map(fc, null, x, y);
      }
    });

    doDragAndDrop(Geometry.min(Geometry.centerPoint(r1), slightOffset), r2);
  }

  /**
   *
   */
  private static void doDragAndDrop(final Point source, final Point dest) {
    log.info(MessageFormat.format(
        "Drag-and-dropping from ({0},{1}) to ({2},{3})", source.x, source.y,
        dest.x, dest.y));
    try {
      final Robot awtRobot = new Robot();
      awtRobot.setAutoDelay(robotDelay);
      // the x+10 motion is needed to let native functions register a drag
      // detect. It did not work under Windows
      // otherwise and has been reported to be required for linux, too.
      // But I could not test that.
      UIThreadRunnable.syncExec(new VoidResult() {

        public void run() {
          awtRobot.mouseMove(source.x, source.y);
          awtRobot.mousePress(InputEvent.BUTTON1_MASK);
          awtRobot.mouseMove((source.x + 10), source.y);
        }
      });

      // now pause the test until all runnables on the Display thread have
      // run this is necessary for the pick up
      // to register on linux
      waitForIdle(awtRobot);

      UIThreadRunnable.syncExec(new VoidResult() {

        public void run() {
          awtRobot.mouseMove((dest.x + 10), dest.y);
          awtRobot.mouseMove(dest.x, dest.y);
        }
      });

      waitForIdle(awtRobot);

      UIThreadRunnable.syncExec(new VoidResult() {

        public void run() {
          awtRobot.mouseRelease(InputEvent.BUTTON1_MASK);
        }
      });
      waitForIdle(awtRobot);
    } catch (final AWTException e) {
      log.info(e.getMessage(), e);
      throw new RuntimeException(e);
    }
    try {
      Thread.sleep(1000);
    } catch (InterruptedException e) {
      // ignore
    }
  }

}
