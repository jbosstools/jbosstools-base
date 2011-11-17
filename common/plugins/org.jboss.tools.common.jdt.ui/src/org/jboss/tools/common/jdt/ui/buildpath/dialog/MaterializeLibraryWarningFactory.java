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
package org.jboss.tools.common.jdt.ui.buildpath.dialog;

import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.common.jdt.ui.Messages;

public class MaterializeLibraryWarningFactory implements IMaterializeLibraryWarningFactory {

	@Override
	public String getWarning(IClasspathContainer classpathLibrary) {
		if (isMavenLibrary(classpathLibrary)) {
			return Messages.Maven_Configuration_Warning;
		} else if (isJreLibrary(classpathLibrary)) {
			return NLS.bind(Messages.Jre_Warning, classpathLibrary.getDescription());
		}
		return null;
	}

	@Override
	public String getDialogWarning(IClasspathContainer classpathLibrary) {
		if (isMavenLibrary(classpathLibrary)) {
			return Messages.Maven_Configuration_Dialog_Warning;
		} else if (isJreLibrary(classpathLibrary)) {
			return NLS.bind(Messages.Jre_Dialog_Warning, classpathLibrary.getDescription());
		}
		return null;
	}

	private boolean isJreLibrary(IClasspathContainer classpathLibrary) {
		return applies(classpathLibrary, "org.eclipse.jdt.launching.JRE_CONTAINER");
	}

	private boolean isMavenLibrary(IClasspathContainer classpathLibrary) {
		return applies(classpathLibrary, "org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER");
	}

	private boolean applies(IClasspathContainer classpathLibrary, String libPrefix) {
		return classpathLibrary != null
				&& classpathLibrary.getPath() != null
				&& classpathLibrary.getPath().toPortableString().startsWith(libPrefix);
	}

}
