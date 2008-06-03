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

import org.jboss.tools.common.model.ui.dnd.ModelTransfer;

public class DropCommandFactory {

	private static final DropCommandFactory INSTANCE = new DropCommandFactory();
	
	public static final IDropCommand PLAIN_MIME_COMMAND = new PlainTextDropCommand();
	public static final IDropCommand UNKNOWN_MIME_COMMAND = new UnknownDropCommand();
	
	public static final String UNKNOWN_MIME_TYPE = "exadel/unknown";
	
	public static final HashMap<String,String> fMimeCommandMap = new HashMap<String,String>();

	////see nsITransferable
	public static final String kFileMime = "application/x-moz-file";
	public static final String kURLMime = "text/x-moz-url";
	public static final String kUnicodeMime = "text/unicode";
	
	static String PACKAGE = "org.jboss.tools.common.model.ui.editors.dnd.";

	static {
		fMimeCommandMap.put(
			////nsITransferable.kFileMime
				kFileMime, PACKAGE + "FileDropCommand"	
		);
		fMimeCommandMap.put(
			////nsITransferable.kURLMime
				kURLMime, PACKAGE + "FileDropCommand"	
		);
		fMimeCommandMap.put(
			ModelTransfer.MODEL, PACKAGE + "PaletteDropCommand"
		);
		fMimeCommandMap.put(
			"text/plain", PACKAGE + "PlainTextDropCommand"	
		);
		fMimeCommandMap.put(
			////nsITransferable.kUnicodeMime
			kUnicodeMime, PACKAGE + "PlainTextDropCommand"	
		);
		fMimeCommandMap.put(
			UNKNOWN_MIME_TYPE, PACKAGE + "UnknownDropCommand"
		);
	}
		
	public static DropCommandFactory getInstance() {
		return INSTANCE;
	}

    public IDropCommand getDropCommand(String mimeType, ITagProposalFactory tagProposalFactory) {
    	IDropCommand fInstance = null;
		try {
			String fClassName = (String)fMimeCommandMap.get(mimeType);
			Class newClass = this.getClass().getClassLoader().loadClass(fClassName);
			fInstance = (IDropCommand)newClass.newInstance();
			fInstance.setTagProposalFactory(tagProposalFactory);
		} catch (Exception e) {
			return UNKNOWN_MIME_COMMAND;
		}
		return fInstance;
    }
}
