package org.jboss.tools.ui.bot.ext.view;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTOpenExt;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.SWTUtilExt;
import org.jboss.tools.ui.bot.ext.gen.IView;
/**
 * base class for all view extensions
 * @author lzoubek@redhat.com
 *
 */
public class ViewBase {
	/**
	 * view object representing current view, MUST be defined in derived
	 * constructor (for use, see {@link SWTOpenExt#viewOpen(IView)}
	 */
	protected IView viewObject;
	protected final SWTOpenExt open;
	protected final SWTUtilExt util;
	protected final SWTBotExt bot;

	protected Logger log = Logger.getLogger(ViewBase.class);

	public ViewBase() {
		open = SWTTestExt.open;
		util = SWTTestExt.util;
		bot = SWTTestExt.bot;

	}
	/**
	 * gets view bot, view is guaranteed to be shown and focused
	 * @return
	 */
	public SWTBot bot() {
		return show().bot();
	}
	/**
	 * shows view
	 */
	public SWTBotView show() {
		return open.viewOpen(viewObject);
	}

}
