package org.jboss.tools.ui.bot.ext.config.requirement;

import java.util.List;
import java.util.Vector;

import org.eclipse.swtbot.eclipse.finder.widgets.SWTBotView;
import org.jboss.tools.ui.bot.ext.SWTTestExt;
import org.jboss.tools.ui.bot.ext.gen.IView;
import org.jboss.tools.ui.bot.ext.types.IDELabel;

/**
 * this is a special requirement which should run only once, main aim of this is  to prepare
 * and setup views behavior (e.g. force Console View not to steal focus) 
 * @author lzoubek@redhat.com
 *
 */
public class PrepareViews extends RequirementBase {

	@Override
	public boolean checkFulfilled() {
		return SWTTestExt.configuredState.isViewsPrepared();
	}

	@Override
	public void handle() {		
		SWTTestExt.open.viewClose(new IView(){
			public List<String> getGroupPath() {
				return new Vector<String>();
			}
			public String getName() {
				return IDELabel.View.WELCOME;
			}});
		SWTTestExt.configuredState.setViewsPrepared(true);

	}

}
