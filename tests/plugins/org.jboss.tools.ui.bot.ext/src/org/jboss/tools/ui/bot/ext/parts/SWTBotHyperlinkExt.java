package org.jboss.tools.ui.bot.ext.parts;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBot;
import org.eclipse.swtbot.swt.finder.widgets.AbstractSWTBotControl;
import org.eclipse.ui.forms.widgets.Hyperlink;
/**
 * 
 * @author lzoubek
 *
 */
public class SWTBotHyperlinkExt extends AbstractSWTBotControl<Hyperlink> {

	public SWTBotHyperlinkExt(Hyperlink w) throws WidgetNotFoundException {
		super(w);
	}

	/**
	 * activates hyper-link by sending special key do widget
	 * NOTE : this does not work well when SWTBot is running inside VNC
	 * @return
	 */
	public AbstractSWTBot<Hyperlink> activate() {
		setFocus();
		keyboard().typeCharacter('\r');
		return this;
	}
	/**
	 * clicks on hyper-link (not real click, but {@link SWTBotHyperlinkExt#activate()}
	 */
	public AbstractSWTBot<Hyperlink> click() {
		return activate();

	}

}
