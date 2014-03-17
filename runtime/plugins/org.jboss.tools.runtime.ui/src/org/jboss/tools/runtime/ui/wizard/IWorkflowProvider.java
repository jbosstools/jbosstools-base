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
package org.jboss.tools.runtime.ui.wizard;

import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;

public interface IWorkflowProvider {
	/**
	 * Can this provider contribute workflow pages for this runtime?
	 * @param dr
	 * @return
	 */
	public boolean canProvideWorkflow(DownloadRuntime dr);
	
	/**
	 * Get a list of child fragments for the given download runtime
	 * @param dr
	 * @return
	 */
	public WizardFragment[] createFragmentsForRuntime(DownloadRuntime dr);
}
