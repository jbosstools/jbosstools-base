package org.jboss.tools.ui.bot.ext.view;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.exceptions.WidgetNotFoundException;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.ListResult;
import org.eclipse.swtbot.swt.finder.utils.SWTUtils;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarDropDownButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarPushButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarRadioButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarSeparatorButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotToolbarToggleButton;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.internal.WorkbenchPartReference;
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

	public List<SWTBotToolbarButton> getToolbarButtons() {
		return UIThreadRunnable.syncExec(new ListResult<SWTBotToolbarButton>() {

			public List<SWTBotToolbarButton> run() {
				SWTBotView view = show();
				IWorkbenchPart obj = ((WorkbenchPartReference) view.getReference()).getPart(false);
				ToolBar toolbar = null;
				IToolBarManager t = ((IViewSite)obj.getSite()).getActionBars().getToolBarManager();
				if (t instanceof ToolBarManager) {
				    toolbar = ((ToolBarManager)t).getControl();
				}

				final List<SWTBotToolbarButton> l = new ArrayList<SWTBotToolbarButton>();

				if (toolbar == null)
					return l;

				ToolItem[] items = toolbar.getItems();
				for (int i = 0; i < items.length; i++) {
					try {
						if (SWTUtils.hasStyle(items[i], SWT.PUSH))
							l.add(new SWTBotToolbarPushButton(items[i]));
						else if(SWTUtils.hasStyle(items[i], SWT.CHECK))
							l.add(new SWTBotToolbarToggleButton(items[i]));
						else if(SWTUtils.hasStyle(items[i], SWT.RADIO))
							l.add(new SWTBotToolbarRadioButton(items[i]));
						else if(SWTUtils.hasStyle(items[i], SWT.DROP_DOWN))
							l.add(new SWTBotToolbarDropDownButton(items[i]));
						else if(SWTUtils.hasStyle(items[i], SWT.SEPARATOR))
							l.add(new SWTBotToolbarSeparatorButton(items[i]));
					} catch (WidgetNotFoundException e) {
						e.printStackTrace();
					}
				}

				return l;

			}
		});
	}
}
