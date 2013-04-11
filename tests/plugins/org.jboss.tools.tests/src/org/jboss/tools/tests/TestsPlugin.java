/******************************************************************************* 
 * Copyright (c) 2007-2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.tests;

import org.eclipse.ui.internal.IPreferenceConstants;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.util.PrefUtil;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * @author eskimo
 *
 */
public class TestsPlugin extends AbstractUIPlugin {

	
	public static final String ID = "org.jboss.tools.tests"; 
	/**
	 * 
	 */
	public TestsPlugin() {
		
	}
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		WorkbenchPlugin.getDefault().getPreferenceStore().setValue(IPreferenceConstants.WORKBENCH_SAVE_INTERVAL, 0);
		PrefUtil.savePrefs();
	}

}
