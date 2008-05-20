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
package org.jboss.tools.common.model.project;

import org.jboss.tools.common.model.XModel;

public interface IModelNature {
	static String PROJECT_FILE_NANE = "org.jboss.tools.jst.web.xml";
	static String PROJECT_FILE = ".settings/" + PROJECT_FILE_NANE;
	static String PROJECT_TEMP = "org.jboss.tools.common.model.temp";
	
	static String ECLIPSE_PROJECT = "eclipse.project";
	static String ECLIPSE_PROJECT_OLD = "exadel.eclipse.project";

	XModel getModel();
	String getID();
}
