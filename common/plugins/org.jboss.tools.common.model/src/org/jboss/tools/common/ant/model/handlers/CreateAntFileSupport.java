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
package org.jboss.tools.common.ant.model.handlers;

import org.jboss.tools.common.model.files.handlers.CreateFileSupport;

public class CreateAntFileSupport extends CreateFileSupport {

	protected String modifyBody(String body) {
		if(body == null || body.length() == 0) {
			body = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
                   "<project basedir=\".\" default=\"build\">\n" +
                   " <target name=\"build\">\n\n" +
                   " </target>\n" +
                   "</project>\n";
		}
		return body;
	}

}
