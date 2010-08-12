package org.jboss.tools.ui.bot.ext.view;

import org.jboss.tools.ui.bot.ext.gen.ActionItem.View.JavaPackageExplorer;

/**
 * Class provides bot routines related to Package Explorer View
 * @author jpeterka
 *
 */
public class PackageExplorer extends ExplorerBase {

	public PackageExplorer() {
		viewObject = JavaPackageExplorer.LABEL;
	}
}
