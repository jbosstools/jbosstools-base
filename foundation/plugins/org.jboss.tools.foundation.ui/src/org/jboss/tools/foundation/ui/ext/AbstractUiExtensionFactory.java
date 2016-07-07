/*******************************************************************************
 * Copyright (c) 2016 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.ui.ext;

import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.e4.ui.model.application.MApplication;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.core.ext.AbstractExtensionFactory;

/**
 * Extension factory for creating extensions from the {@link MApplication} context.
 */
public abstract class AbstractUiExtensionFactory extends AbstractExtensionFactory {
	/**
	 * @return the application context.
	 */
	protected IEclipseContext getContext() {
		return ((MApplication)PlatformUI.getWorkbench().getService(MApplication.class)).getContext();
	}
}
