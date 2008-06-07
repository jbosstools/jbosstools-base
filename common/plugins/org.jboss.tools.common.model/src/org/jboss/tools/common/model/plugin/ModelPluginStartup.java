package org.jboss.tools.common.model.plugin;

import org.eclipse.ui.IStartup;
import org.eclipse.ui.PlatformUI;

public class ModelPluginStartup implements IStartup {

	public void earlyStartup() {
		PlatformUI.getWorkbench().addWindowListener(ModelPlugin.getDefault());
	}

}
