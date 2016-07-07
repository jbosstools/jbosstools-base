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
package org.jboss.tools.foundation.ui.test.ext;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.foundation.ui.ext.AbstractUiExtensionFactory;
import org.jboss.tools.foundation.ui.test.Activator;
import org.junit.Assert;
import org.junit.Test;
import org.osgi.framework.Bundle;

public class E4ExtensionFactoryTest {
	
	public static class TestExtensionFactory extends AbstractUiExtensionFactory {
		@Override
		protected Bundle getBundle() {
			return Activator.getDefault().getBundle();
		}
	}
	
	@Test
	public void testExtensionFactory() throws CoreException {
		IExtension extension = Platform.getExtensionRegistry().getExtension("org.jboss.tools.foundation.ui.test.view.extension");
		IConfigurationElement element = extension.getConfigurationElements()[0];
		TestView view = (TestView)element.createExecutableExtension("class");
		Assert.assertTrue(view.isValid());
	}
}
