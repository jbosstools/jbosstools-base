/*******************************************************************************
  * Copyright (c) 2010 - 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
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
