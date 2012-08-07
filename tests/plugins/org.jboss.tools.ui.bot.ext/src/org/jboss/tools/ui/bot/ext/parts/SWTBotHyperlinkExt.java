package org.jboss.tools.ui.bot.ext.parts;

import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.results.BoolResult;
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
		int timeout = 0;
		while (!hasFocus() && timeout!=5){
			//try to set focus
			setFocus();
			log.info("Trying to set focus");
			sleep(1000);
			timeout++;
		}
		if (!hasFocus()){
			throw new IllegalStateException("Unable to focus widget of type Hyperlink");
		}
		keyboard().typeCharacter('\r');
		return this;
	}
	
	
	/**
	 * Tests whether widget has focus or not. Needed for workaround of issue, where method setFocus() isn't working properly when ececuting test via maven.
	 * @return true if widget has focus. False otherwise.
	 */
	
	public boolean hasFocus(){
		return syncExec(new BoolResult() {
			
			@Override
			public Boolean run() {
				return widget.isFocusControl();
			}
		});
	}

	/**
	 * clicks on hyper-link (not real click, but {@link SWTBotHyperlinkExt#activate()}
	 */
	public AbstractSWTBot<Hyperlink> click() {
		return activate();

	}

}
