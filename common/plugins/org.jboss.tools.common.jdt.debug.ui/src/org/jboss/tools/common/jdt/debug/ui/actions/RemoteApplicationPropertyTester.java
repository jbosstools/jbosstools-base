/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.ui.actions;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.VmModel;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteApplicationPropertyTester extends PropertyTester {

	@Override
	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		if ("remoteCount".equals(property)) { //$NON-NLS-1$
			VmModel[] models = RemoteDebugActivator.getDefault().getDebugModels(new NullProgressMonitor());
			if (expectedValue instanceof Integer) {
				int value = (Integer) expectedValue;
				if (models != null) {
					int count = models.length;
					return count >= value;
				}
			}
		}
		if ("remoteExists".equals(property)) { //$NON-NLS-1$
			if (expectedValue instanceof Boolean) {
				Boolean value = (Boolean) expectedValue;
				if (value) {
					VmModel[] models = RemoteDebugActivator.getDefault().getDebugModels(new NullProgressMonitor());
					boolean exists = models != null && models.length > 0;
					return exists;
				}
			}
		}
		return false;
	}

}
