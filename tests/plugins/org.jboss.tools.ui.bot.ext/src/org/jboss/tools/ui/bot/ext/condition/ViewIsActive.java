package org.jboss.tools.ui.bot.ext.condition;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.utils.StringUtils;
import org.eclipse.swtbot.swt.finder.utils.internal.Assert;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;

/**
 * SWTBot Condition checking if a view is active
 * @author jjankovi
 *
 */
public class ViewIsActive implements ICondition{
	
	private String viewTitle;	

	public ViewIsActive(String viewTitle) {
		Assert.isNotNull(viewTitle, "View title text was null");
		Assert.isLegal(!StringUtils.isEmpty(viewTitle), "View title text was empty");
		this.viewTitle = viewTitle;
	}

	public String getFailureMessage() {
		return "View '" + viewTitle + "' is not active";
	}

	public boolean test() throws Exception {
		try {
			SWTBotView view = SWTBotFactory.getBot().viewByTitle(viewTitle);			
			return view.isActive();
		} catch (WidgetNotFoundException e) {
		}
		return false;
	}

	@Override
	public void init(SWTBot bot) {	
	}
}
