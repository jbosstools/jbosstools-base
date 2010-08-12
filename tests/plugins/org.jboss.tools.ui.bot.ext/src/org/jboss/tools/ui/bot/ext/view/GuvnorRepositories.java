package org.jboss.tools.ui.bot.ext.view;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.gen.ActionItem.View.GuvnorGuvnorRepositories;

/**
 * Class provides bot routines related to Repositories View
 * @author Vlado Pakan
 *
 */
public class GuvnorRepositories extends ExplorerBase {
  Logger log = Logger.getLogger(GuvnorRepositories.class);
	public GuvnorRepositories() {
		viewObject = GuvnorGuvnorRepositories.LABEL;
	}
}
