/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard.workflow;

import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.wizard.IWorkflowProvider;

/**
 * Create the wizard fragments for workflow participation in the UI
 */
public class DownloadManagerWorkflowProvider implements IWorkflowProvider {

	@Override
	public boolean canProvideWorkflow(DownloadRuntime dr) {
		Object requiresCreds = dr.getProperty(DownloadRuntime.PROPERTY_REQUIRES_CREDENTIALS);
		if( requiresCreds != null && Boolean.parseBoolean(requiresCreds.toString())) {
			return true;
		}
		return false;
	}

	@Override
	public WizardFragment[] createFragmentsForRuntime(DownloadRuntime dr) {
		if( canProvideWorkflow(dr))
			return new WizardFragment[]{new DownloadManagerCredentialsFragment()};
		return new WizardFragment[0];
	}

}
