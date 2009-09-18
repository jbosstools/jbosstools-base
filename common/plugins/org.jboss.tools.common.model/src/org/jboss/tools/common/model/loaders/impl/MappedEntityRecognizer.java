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
package org.jboss.tools.common.model.loaders.impl;

import java.util.*;

import org.jboss.tools.common.meta.XMapping;
import org.jboss.tools.common.meta.impl.XModelMetaDataImpl;
import org.jboss.tools.common.model.loaders.*;

public class MappedEntityRecognizer implements EntityRecognizer {
	static String MAPPED_ENTITIES = "MappedEntities"; //$NON-NLS-1$
    private Map<String,String> map = new HashMap<String,String>();

	public MappedEntityRecognizer() {
		XMapping m = XModelMetaDataImpl.getInstance().getMapping(MAPPED_ENTITIES);
		if(m != null) {
			String[] ks = m.getKeys();
			for (int i = 0; i < ks.length; i++) {
				String extension = ks[i];
				String entity = m.getValue(extension);
				map.put(extension, entity);
			}
		}
    }

	// NB i18n: there is code that depends on these entity names (in English)
    public String getEntityName(String ext, String body) {
    	if(ext == null) return null;
        String s = (String)map.get(ext.toLowerCase());
        return (s != null) ? s : "File" + ext.toUpperCase(); //$NON-NLS-1$
    }

}
