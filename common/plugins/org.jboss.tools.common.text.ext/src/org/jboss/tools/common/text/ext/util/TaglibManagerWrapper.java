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
package org.jboss.tools.common.text.ext.util;

import java.util.List;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jst.jsp.core.internal.contentmodel.TaglibController;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.TLDCMDocumentManager;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.TaglibTracker;

public class TaglibManagerWrapper {
	TLDCMDocumentManager manager;
	List trackers;

	public TaglibManagerWrapper() {}
	
	public void init(IDocument document, int offset) {
		manager = TaglibController.getTLDCMDocumentManager(document);
		if(exists()) {
			trackers = manager.getCMDocumentTrackers(offset);
		}
	}
	
	public boolean exists() {
		return manager != null;
	}
	
	public String getUri(String prefix) {
		for (int i = 0; i < trackers.size(); i++) {
			TaglibTracker tt = (TaglibTracker)trackers.get(i);
			if (prefix.equals(tt.getPrefix())) {
				return tt.getURI();
			}
		}
		return null;
	}

	public String getPrefix(String uri) {
		for (int i = 0; i < trackers.size(); i++) {
			TaglibTracker tt = (TaglibTracker)trackers.get(i);
			if (uri.equals(tt.getURI())) {
				return tt.getPrefix();
			}
		}
		return null;
	}

	public String getCorePrefix() {
		return getPrefix("http://java.sun.com/jsf/core");
	}

}
