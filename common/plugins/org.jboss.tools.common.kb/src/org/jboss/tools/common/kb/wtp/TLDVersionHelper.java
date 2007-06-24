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
package org.jboss.tools.common.kb.wtp;

import java.util.List;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jst.jsp.core.internal.contentmodel.TaglibController;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.CMDocumentImpl;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.TLDCMDocumentManager;
import org.eclipse.jst.jsp.core.internal.contentmodel.tld.TaglibTracker;
import org.eclipse.wst.xml.core.internal.contentmodel.CMDocument;

public class TLDVersionHelper {
	public static String getTldVersion(String uri, String prefix, IDocument document) {
		if(uri==null & prefix==null && document==null) {
			return null;
		}
		TLDCMDocumentManager manager = TaglibController.getTLDCMDocumentManager(document);
		if (manager != null) {
			List list = manager.getTaglibTrackers();
			for(int i=0; i<list.size(); i++) {
				TaglibTracker tracker = (TaglibTracker)list.get(i);
				if(prefix.equals(tracker.getPrefix()) && uri.equals(tracker.getURI())) {
					return getTldVersion(tracker);
				}
			}
		}
		return null;
	}

	public static String getTldVersion(TaglibTracker tracker) {
		CMDocument cmd = tracker.getDocument();
		if(cmd!=null && cmd instanceof CMDocumentImpl) {
			CMDocumentImpl doc = (CMDocumentImpl)cmd;
			return doc.getTlibversion();
		}
		return null;
	}

}
