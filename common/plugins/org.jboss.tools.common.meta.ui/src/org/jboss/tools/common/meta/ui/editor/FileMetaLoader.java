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
package org.jboss.tools.common.meta.ui.editor;

import org.jboss.tools.common.model.loaders.impl.SimpleWebFileLoader;
import org.jboss.tools.common.model.util.*;

public class FileMetaLoader extends SimpleWebFileLoader {

    public FileMetaLoader() {}

	protected boolean isCheckingDTD() {
		return false;
	}

    protected boolean isCheckingSchema() {
    	return false;
    }

	protected XModelObjectLoaderUtil createUtil() {
		return new MetaLoaderUtil();
	}

}
