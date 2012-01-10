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

import static org.jboss.tools.common.jdt.core.buildpath.ClasspathContainersHelper.applies;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.osgi.util.NLS;
import org.jboss.tools.common.jdt.core.buildpath.ClasspathContainersHelper;
import org.jboss.tools.common.jdt.ui.Messages;

public class MaterializeLibraryWarningFactory implements IMaterializeLibraryWarningFactory {

	@Override
	public String getWarning(IClasspathContainer classpathLibrary) {
		if (isMavenLibrary(classpathLibrary)) {
			return Messages.Maven_Configuration_Warning;
		} 
		if (isJreLibrary(classpathLibrary)) {
			return NLS.bind(Messages.Jre_Warning, classpathLibrary.getDescription());
		}
		if (isGradleLibrary(classpathLibrary)) {
			return Messages.Gradle_Configuration_Warning;
		}
		return null;
	}

	@Override
	public String getDialogWarning(IClasspathContainer classpathLibrary) {
		if (isMavenLibrary(classpathLibrary)) {
			return Messages.Maven_Configuration_Dialog_Warning;
		}  
		if (isJreLibrary(classpathLibrary)) {
			return NLS.bind(Messages.Jre_Dialog_Warning, classpathLibrary.getDescription());
		}
		if (isGradleLibrary(classpathLibrary)) {
			return Messages.Gradle_Configuration_Dialog_Warning;
		}
		return null;
	}

	private boolean isJreLibrary(IClasspathContainer classpathLibrary) {
		return applies(classpathLibrary, ClasspathContainersHelper.JRE_CONTAINER_ID);
	}

	private boolean isMavenLibrary(IClasspathContainer classpathLibrary) {
		return applies(classpathLibrary, ClasspathContainersHelper.MAVEN_CONTAINER_ID);
	}

	private boolean isGradleLibrary(IClasspathContainer classpathLibrary) {
		return applies(classpathLibrary, ClasspathContainersHelper.GRADLE_CONTAINER_ID);
	}

}
