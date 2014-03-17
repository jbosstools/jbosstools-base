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
package org.jboss.tools.runtime.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.ui.wizard.IWorkflowProvider;

/**
 * Loads all extensions declared by this plugin
 */
public class RuntimeUIExtensionManager {
	private static HashMap<String, IWorkflowProvider> providerMap = null;
	public static WizardFragment[] createFragmentsForRuntime(DownloadRuntime dr) {
		ensureLoaded();
		ArrayList<WizardFragment> toReturn = new ArrayList<WizardFragment>();
		
		Set<String> keys = providerMap.keySet();
		Iterator<String> keyIt = keys.iterator();
		while(keyIt.hasNext()) {
			IWorkflowProvider p = providerMap.get(keyIt.next());
			if( p.canProvideWorkflow(dr)) {
				WizardFragment[] fragList = p.createFragmentsForRuntime(dr);
				if( fragList != null )
					toReturn.addAll(Arrays.asList(fragList));
			}
		}
		return (WizardFragment[]) toReturn.toArray(new WizardFragment[toReturn.size()]);
	}
	
	/*
	 * Load the extension point
	 */
	private static void ensureLoaded() {
		if( providerMap != null )
			return;
		
		providerMap = new HashMap<String, IWorkflowProvider>();
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IConfigurationElement[] cf = registry.getConfigurationElementsFor(RuntimeUIActivator.PLUGIN_ID, "workflowProvider"); //$NON-NLS-1$
		for( int i = 0; i < cf.length; i++ ) {
			try {
				String id = cf[i].getAttribute("id");
				Object o = cf[i].createExecutableExtension("class");
				providerMap.put(id, (IWorkflowProvider)o);
			} catch(CoreException ce) {
				RuntimeUIActivator.getDefault().getLog().log(ce.getStatus());
			}
		}
	}
	
}
