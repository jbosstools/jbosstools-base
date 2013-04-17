package org.jboss.tools.common.model.test;

import org.eclipse.ui.plugin.AbstractUIPlugin;

public class ModelTestPlugin extends AbstractUIPlugin {
	
	private static ModelTestPlugin INSTANCE;
	
	public ModelTestPlugin() {
		super();
		INSTANCE = this;
	}

	public static ModelTestPlugin getDefault() {
		return INSTANCE;
	}
	
}
