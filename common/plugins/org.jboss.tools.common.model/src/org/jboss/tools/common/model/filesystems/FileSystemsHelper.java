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
package org.jboss.tools.common.model.filesystems;

import org.jboss.tools.common.model.*;

public class FileSystemsHelper {
	
	public static XModelObject getFileSystems(XModel model) {
		return model.getByPath("FileSystems");
	}

	public static XModelObject getFileSystem(XModel model, String name) {
		return model.getByPath("FileSystems/" + name);
	}
	
	public static XModelObject getWebRoot(XModel model) {
		return model.getByPath("FileSystems/WEB-ROOT");
	}

	public static XModelObject getWebInf(XModel model) {
		return model.getByPath("FileSystems/WEB-INF");
	}

}
