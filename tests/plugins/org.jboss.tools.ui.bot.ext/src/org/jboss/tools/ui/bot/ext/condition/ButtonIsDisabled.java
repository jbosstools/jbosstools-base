package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.utils.StringUtils;
import org.eclipse.swtbot.swt.finder.utils.internal.Assert;
import org.eclipse.swtbot.swt.finder.waits.DefaultCondition;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;

/**
 * SWTBot Condition checking if a button is disabled
 * @author jpeterka
 *
 */
public class ButtonIsDisabled extends DefaultCondition {

	private String text;

	public ButtonIsDisabled(String text) {
		Assert.isNotNull(text, "The button text was null"); //$NON-NLS-1$
		Assert.isLegal(!StringUtils.isEmpty(text), "The button text was empty"); //$NON-NLS-1$
		this.text = text;
	}

	public String getFailureMessage() {
		return "The button '" + text + "' is not disabled"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	public boolean test() throws Exception {
		try {
			SWTBotButton button = bot.buttonWithLabel(text);
			return !button.isEnabled();
		} catch (WidgetNotFoundException e) {
		}
		return false;
	}
}
