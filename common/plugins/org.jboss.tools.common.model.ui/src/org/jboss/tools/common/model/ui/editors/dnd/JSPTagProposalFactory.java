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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.util.HashMap;
import java.util.Map;

public class JSPTagProposalFactory implements ITagProposalFactory {
	private static final JSPTagProposalFactory INSTANCE = new JSPTagProposalFactory(); 
	public static Map<String,String> loaderMap = new HashMap<String,String>(); 

	static {
		loaderMap.put(DropCommandFactory.kFileMime, DropCommandFactory.PACKAGE + "FileTagProposalLoader");		
		loaderMap.put(DropCommandFactory.kURLMime, DropCommandFactory.PACKAGE + "FileTagProposalLoader");		
	}
	
	public static JSPTagProposalFactory getInstance() {
		return INSTANCE;
	}
	
    private JSPTagProposalFactory() {
	}
    
    public ITagProposalLoader getProposalLoader(String mimeType) {
    	ITagProposalLoader fInstance = null;
		try {
			String fClassName = (String)loaderMap.get(mimeType);
			Class newClass = this.getClass().getClassLoader().loadClass(fClassName);
			fInstance = (ITagProposalLoader)newClass.newInstance();
		} catch (Exception e) {
			return DEFAULT_PROPOSAL_LOADER;
		}
		return fInstance;
    }
}

