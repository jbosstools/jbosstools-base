package org.jboss.tools.ui.bot.ext.parts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.results.IntResult;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.eclipse.swtbot.swt.finder.utils.MessageFormat;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
/**
 * SWTBot Scale component (very similar to slider, can be found for example in 'New Web Service' wizard)
 * @author lzoubek
 *
 */
public class SWTBotScaleExt extends AbstractSWTBotControl<Scale> {

	public SWTBotScaleExt(Scale w) throws WidgetNotFoundException {
		super(w);
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * Return the current value of the slider.
	 *
	 * @return the current selection in the slider.
	 */
	public int getSelection() {
		return syncExec(new IntResult() {
			public Integer run() {
				return widget.getSelection();
			}
		});
	}

	/**
	 * Set the selection to the specified value.
	 *
	 * @param value the value to set into the slider.
	 */
	public void setSelection(final int value) {
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
	 * Return the maximum value the slider will accept.
	 *
	 * @return the maximum of the slider.
	 */
	public int getMaximum() {
		return syncExec(new IntResult() {
			public Integer run() {
				return widget.getMaximum();
			}
		});
	}

	/**
	 * Return the minimum value the slider will accept.
	 *
	 * @return the minimum of the slider.
	 */
	public int getMinimum() {
		return syncExec(new IntResult() {
			public Integer run() {
				return widget.getMinimum();
			}
		});
	}

	/**
	 * Return the increment of the slider.
	 *
	 * @return the increment of the slider.
	 */
	public int getIncrement() {
		return syncExec(new IntResult() {
			public Integer run() {
				return widget.getIncrement();
			}
		});
	}

	/**
	 * Return the page increment of the slider.
	 *
	 * @return the increment of the slider.
	 */
	public int getPageIncrement() {
		return syncExec(new IntResult() {
			public Integer run() {
				return widget.getPageIncrement();
			}
		});
	}

}
