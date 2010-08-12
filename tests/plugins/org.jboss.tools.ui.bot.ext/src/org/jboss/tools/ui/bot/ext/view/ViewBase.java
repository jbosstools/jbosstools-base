package org.jboss.tools.ui.bot.ext.view;

import org.apache.log4j.Logger;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.jboss.tools.ui.bot.ext.SWTBotExt;
import org.jboss.tools.ui.bot.ext.SWTOpenExt;
import org.jboss.tools.ui.bot.ext.gen.IView;
/**
 * base class for all view extensions
 * @author lzoubek@redhat.com
 *
 */
public class ViewBase extends SWTBotExt {
	/**
	 * view object representing current view, MUST be defined in derived
	 * constructor (for use, see {@link SWTOpenExt#viewOpen(IView)}
	 */
	protected IView viewObject;
	protected final SWTOpenExt open;
	Logger log = Logger.getLogger(ViewBase.class);

	public ViewBase() {
		open = new SWTOpenExt(this);
	}

	/**
	 * shows view
	 */
	public SWTBotView show() {
		return open.viewOpen(viewObject);
	}

}
