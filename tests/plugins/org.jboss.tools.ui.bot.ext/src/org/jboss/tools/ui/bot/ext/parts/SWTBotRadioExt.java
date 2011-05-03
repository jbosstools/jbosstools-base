 /*******************************************************************************
  * Copyright (c) 2007-2011 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.parts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.results.BoolResult;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.utils.MessageFormat;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotRadio;
/**
 * SWTBot Radio Button component
 * @author vpakan
 *
 */
public class SWTBotRadioExt extends AbstractSWTBotControl<Button> {

  private SWTBotRadio botRadio = null;
  
	public SWTBotRadioExt(Button radioButton) throws WidgetNotFoundException {
		super(radioButton);
		this.botRadio = new SWTBotRadio(radioButton);
	}
	
	/**
	 * Return the current value of the slider.
	 *
	 * @return the current selection in the slider.
	 */
	public boolean getSelection() {
		return syncExec(new BoolResult() {
			public Boolean run() {
				return widget.getSelection();
			}
		});
	}

	/**
	 * Set the selection to the specified value.
	 *
	 * @param value the value to set into the slider.
	 */
	public void setSelection(final boolean value) {
		log.debug(MessageFormat.format("Setting selection on {0} to {1}", this, value)); //$NON-NLS-1$
		waitForEnabled();
		asyncExec(new VoidResult() {
			public void run() {
				widget.setSelection(value);
			}
		});
		notify(SWT.Selection);
		log.debug(MessageFormat.format("Set selection on {0} to {1}", this, value)); //$NON-NLS-1$
	}
  
	 /**
   * Clicks on the radio button without firing deselection event for previously selected radio button.
   */
  public SWTBotRadio clickWithoutDeselectionEvent() {
    if (botRadio.isSelected()) {
      log.debug(MessageFormat.format("Widget {0} is already selected, not clicking again.", this)); //$NON-NLS-1$
      return botRadio;
    }
    waitForEnabled();

    log.debug(MessageFormat.format("Clicking on {0}", this)); //$NON-NLS-1$

    notify(SWT.Activate);
    notify(SWT.MouseDown, createMouseEvent(0, 0, 1, 0, 1));
    notify(SWT.MouseUp, createMouseEvent(0, 0, 1, SWT.BUTTON1, 1));
    asyncExec(new VoidResult() {
      public void run() {
        widget.setSelection(true);
      }
    });
    notify(SWT.Selection);
    log.debug(MessageFormat.format("Clicked on {0}", this)); //$NON-NLS-1$
    return botRadio;
  }
}
