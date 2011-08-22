/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.validation;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.log.BaseUIPlugin;
import org.jboss.tools.common.log.IPluginLog;
import org.osgi.framework.BundleContext;

/**
 * @author Alexey Kazakov
 */
public class CommonValidationPlugin extends BaseUIPlugin {

	public static final String PLUGIN_ID = "org.jboss.tools.common.validation"; //$NON-NLS-1$
	protected static CommonValidationPlugin INSTANCE;

	public CommonValidationPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		INSTANCE = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		INSTANCE = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static CommonValidationPlugin getDefault() {
		return INSTANCE;
	}

	/**
	 * @return IPluginLog object
	 */
	public static IPluginLog getPluginLog() {
		return getDefault();
	}

    public static boolean makeBuilderLast(IProject project, String builderId) throws CoreException {
		IProjectDescription d = project.getDescription();
		ICommand[] bs = d.getBuildSpec();
		ICommand v = null;
		boolean updated = false;
		for (int i = 0; i < bs.length; i++) {
			if(builderId.equals(bs[i].getBuilderName())) {
				v = bs[i];
			}
			if(v != null) {
				if(i + 1 < bs.length) {
					bs[i] = bs[i + 1];
					updated = true;
				} else if(updated) {
					bs[i] = v;
				}
			}
		}
		if(updated) {
			d.setBuildSpec(bs);
			project.setDescription(d, IProject.FORCE, new NullProgressMonitor());
		}
		return updated;
    }
}