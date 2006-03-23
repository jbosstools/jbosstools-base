/*
 * JBoss, Home of Professional Open Source
 * Copyright 2005, JBoss Inc., and individual contributors as indicated
 * by the @authors tag. See the copyright.txt in the distribution for a
 * full listing of individual contributors.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA, or see the FSF site: http://www.fsf.org.
 */
package org.jboss.ide.eclipse.core.test.util;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;

/**
 * Various UI related utility methods useful for Eclipse test cases
 * 
 * @author <a href="marshall@jboss.org">Marshall Culpepper</a>
 * @version $Revision$
 */
public class TestUIUtil
{
    /**
     * Process UI input but do not return for the specified time interval.
     * 
     * @param waitTimeMillis
     *            the number of milliseconds
     */
    public static void delay(long waitTimeMillis) {
        Display display = Display.getCurrent();

        // If this is the UI thread,
        // then process input.
        if (display != null) {
            long endTimeMillis = System.currentTimeMillis() + waitTimeMillis;
            while (System.currentTimeMillis() < endTimeMillis) {
                //if (!display.readAndDispatch())
                    display.sleep();
            }
            display.update();
        }

        // Otherwise, perform a simple sleep.
        else {
            try {
                Thread.sleep(waitTimeMillis);
            } catch (InterruptedException e) {
                // Ignored.
            }
        }
    }
    
    /**
     * Wait until all background tasks are complete.
     */
    public static void waitForJobs() {
        while (!Platform.getJobManager().isIdle())
            delay(1000);
    }
    
    /**
     * Returns a simple selection provider wrapper for the passed in source
     * 
     * @param source
     * @return
     */
    public static ISelectionProvider getSelectionProvider (final Object source)
    {
       return new ISelectionProvider () {
         public void addSelectionChangedListener(ISelectionChangedListener listener) { }
         public ISelection getSelection() { return new StructuredSelection(source); }
         public void removeSelectionChangedListener(ISelectionChangedListener listener) { }
         public void setSelection(ISelection selection) { }
       };
    }
    
    /**
     *  A convenience method that simulates a user clicking a button
     * 
     * @param button
     */
    public static void clickButton (Button button)
    {
       if (button == null)
          throw new NullPointerException("Button is null");
       
       Event e = new Event ();
       e.data = button;
       e.widget = button;
       
       button.notifyListeners(SWT.Selection, e);
    }
    
    /**
     * This method tries it's best to find the OK and CANCEL buttons
     * within a JFace dialog (since they are hidden away from public API)
     *  
     * @param dialog The dialog to search under
     * @param id Dialog.OK or Dialog.CANCEL
     * @return
     */
    public static Button getDialogButton (Dialog dialog, int id)
    {
       Control controls[] = dialog.getShell().getChildren();
       if (controls.length == 1 && controls[0] instanceof Composite)
       {
          Composite composite = (Composite) controls[0];
          controls = composite.getChildren();
          
          if (controls.length >= 1 && controls[controls.length-1] instanceof Composite)
          {
             Composite buttonComposite = (Composite) controls[controls.length-1];
             controls = buttonComposite.getChildren();
             
             for (int i = 0; i < controls.length; i++)
             {
                if (controls[i] instanceof Button)
                {
                   Button button = (Button) controls[i];
                   // It looks like the button id is stored as the button's user data (as an Integer)
                   
                   int buttonId = ((Integer) button.getData()).intValue();
                   if (buttonId == id) return button;
                }
             }
          }
       }
       
       return null;
    }
}
