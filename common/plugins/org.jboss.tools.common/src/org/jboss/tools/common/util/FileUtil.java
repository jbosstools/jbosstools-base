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
package org.jboss.tools.common.util;

import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.IFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.common.CommonPlugin;

public final class FileUtil extends FileUtils {

    public FileUtil() {}

	/**
	 * Returns the content of the file. If this file is open in an active editor
	 * then the content of the editor will be returned.
	 * 
	 * @param file
	 * @return
	 */
	public static String getContentFromEditorOrFile(IFile file) {
		IFileBuffer b = FileBuffers.getTextFileBufferManager().getFileBuffer(file.getFullPath(), LocationKind.IFILE);
		if (b instanceof ITextFileBuffer) {
			IDocument doc = ((ITextFileBuffer)b).getDocument();
			if(doc != null) {
				return doc.get();
			}
		}
		ITextEditor editor = EclipseUIUtil.getActiveEditor();
		if (editor != null) {
			IEditorInput editorInput = editor.getEditorInput();
			if (editorInput instanceof IFileEditorInput) {
				IFileEditorInput fileInput = (IFileEditorInput) editorInput;
				if (file.equals(fileInput.getFile())) {
					IDocumentProvider dp = editor.getDocumentProvider();
					try {
						dp.connect(fileInput);
						IDocument doc = dp.getDocument(fileInput);
						return doc.get();
					} catch (CoreException e) {
						CommonPlugin.getDefault().logError(e);
					} finally {
						dp.disconnect(fileInput);
					}
				}
			}
		}
		try {
			return FileUtil.readStream(file);
		} catch (CoreException e) {
			CommonPlugin.getDefault().logError(e);
			return null;
		}
	}

	public static boolean isDoctypeHTML(IFile file) {
		return isDoctypeHTML(getContentFromEditorOrFile(file));
	}

	public static boolean isDoctypeHTML(String content) {
		int i = 0;
		while(true) {
			i = content.indexOf("<!", i);
			if(i < 0) break;
			int j = content.indexOf(">", i);
			if(j < 0) break;
			if(j > i + 4 && content.substring(i + 2, i + 4).equals("--")) {
				i = content.indexOf("-->", i + 4);
				if(i < 0) break;
				i += 3;
				continue;
			} else if(content.indexOf("<!", i + 1) > i && content.indexOf("<!", i + 1) < j) {
				i += 2;
				continue;
			} else if(j > i + 13) {
				String dt1 = content.substring(i + 2, i + 9).trim();
				if("doctype".equalsIgnoreCase(dt1)) {
					String dt = content.substring(i + 9, j).trim();
					return dt.equalsIgnoreCase("html");
				}
			}
			break;
		}
		return false;
	}
}