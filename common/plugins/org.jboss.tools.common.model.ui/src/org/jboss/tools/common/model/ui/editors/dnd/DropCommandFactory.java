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
import org.jboss.tools.common.model.util.ModelFeatureFactory;

/**
 * The Class DropCommandFactory.
 */
public class DropCommandFactory {

	/** The Constant INSTANCE. */
	private static final DropCommandFactory INSTANCE = new DropCommandFactory();
	
	/** The Constant PLAIN_MIME_COMMAND. */
	public static final IDropCommand PLAIN_MIME_COMMAND = new PlainTextDropCommand();
	
	/** The Constant UNKNOWN_MIME_COMMAND. */
	public static final IDropCommand UNKNOWN_MIME_COMMAND = new UnknownDropCommand();
	
	/** The Constant UNKNOWN_MIME_TYPE. */
	public static final String UNKNOWN_MIME_TYPE = "exadel/unknown"; //$NON-NLS-1$
	
	/** The Constant fMimeCommandMap. */
	public static final HashMap<String,String> fMimeCommandMap = new HashMap<String,String>();

	////see nsITransferable
	/** The Constant kFileMime. */
	public static final String kFileMime = "application/x-moz-file"; //$NON-NLS-1$
	
	/** The Constant kURLMime. */
	public static final String kURLMime = "text/x-moz-url"; //$NON-NLS-1$
	
	/** The Constant kUnicodeMime. */
	public static final String kUnicodeMime = "text/unicode"; //$NON-NLS-1$
	
	/** The Constant kHtmlText. */
	public static final String kHtmlText   =  "text/html"; //$NON-NLS-1$
	
	/** The PACKAGE. */
	static String PACKAGE = "org.jboss.tools.common.model.ui.editors.dnd."; //$NON-NLS-1$
	
	static String UNKNOWN_DROP_COMMAND = "org.jboss.tools.common.model.ui.editors.dnd.UnknownDropCommand"; //$NON-NLS-1$
	static String TEXT_DROP_COMMAND = "org.jboss.tools.common.model.ui.editors.dnd.PlainTextDropCommand"; //$NON-NLS-1$
	static String FILE_DROP_COMMAND = "org.jboss.tools.jst.jsp.jspeditor.dnd.FileDropCommand"; //$NON-NLS-1$
	static String PALETTE_DROP_COMMAND = "org.jboss.tools.jst.jsp.jspeditor.dnd.PaletteDropCommand"; //$NON-NLS-1$

	static {
		fMimeCommandMap.put(
			////nsITransferable.kFileMime
				kFileMime, FILE_DROP_COMMAND
		);
		fMimeCommandMap.put(
			////nsITransferable.kURLMime
				kURLMime, FILE_DROP_COMMAND
		);
		fMimeCommandMap.put(
			ModelTransfer.MODEL, PALETTE_DROP_COMMAND
		);
		fMimeCommandMap.put(
			"text/plain", TEXT_DROP_COMMAND
		);
		fMimeCommandMap.put(
			////nsITransferable.kUnicodeMime
			kUnicodeMime, TEXT_DROP_COMMAND
		);
		fMimeCommandMap.put(kHtmlText, TEXT_DROP_COMMAND);
		
		fMimeCommandMap.put(
			UNKNOWN_MIME_TYPE, UNKNOWN_DROP_COMMAND
		);
	}
		
	/**
	 * Gets the instance.
	 * 
	 * @return the instance
	 */
	public static DropCommandFactory getInstance() {
		return INSTANCE;
	}

    /**
     * Gets the drop command.
     * 
     * @param mimeType the mime type
     * @param tagProposalFactory the tag proposal factory
     * 
     * @return the drop command
     */
    public IDropCommand getDropCommand(String mimeType, ITagProposalFactory tagProposalFactory) {
    	IDropCommand fInstance = UNKNOWN_MIME_COMMAND;
		String fClassName = (String)fMimeCommandMap.get(mimeType);
		fInstance = (IDropCommand)ModelFeatureFactory.getInstance().createFeatureInstance(fClassName);
		if(fInstance == null) {
			fInstance = new UnknownDropCommand();
		}
		if(fInstance != null) {
			fInstance.setTagProposalFactory(tagProposalFactory);
		}
		return fInstance;
    }
}
