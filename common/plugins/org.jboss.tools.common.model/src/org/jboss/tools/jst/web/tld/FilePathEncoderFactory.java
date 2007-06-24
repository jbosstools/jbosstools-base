/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.jst.web.tld;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;

public class FilePathEncoderFactory {
	static IFilePathEncoder jsfEncoder = null;

	static {
		try {
			jsfEncoder = (IFilePathEncoder)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.jsf.model.helpers.pages.FilePathEncoder");
		} catch (Exception e) {
			ModelPlugin.log(e);
		}
	}

	public static IFilePathEncoder getEncoder(IProject project) {
		if(project == null || !project.isOpen()) return null;
		try {
			if(project.hasNature("org.jboss.tools.jsf.jsfnature")) return jsfEncoder;
		} catch (Exception e) {
			//ignore
		}
		return null;
	}

}
