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
package org.jboss.tools.common.resref.core;

import java.util.List;

import org.jboss.tools.common.resref.messages.Messages;

public class CssReferencesComposite extends ResourceReferencesComposite {

	protected String getEntity() {
		return (file != null) ? "VPECSSReference" : "VPECSSReferenceExt";
	}

	protected ResourceReferencesTableProvider createTableProvider(List dataList) {
		return ResourceReferencesTableProvider.getCSSTableProvider(dataList);
	}

	protected ResourceReferenceList getReferenceList() {
		return CSSReferenceList.getInstance();
	}
	
	
	/**
	 * @see ResourceReferencesComposite#createGroupLabel()
	 */
    @Override
    protected String createGroupLabel() {
        return Messages.INCLUDED_CSS_FILES;
    }


}
