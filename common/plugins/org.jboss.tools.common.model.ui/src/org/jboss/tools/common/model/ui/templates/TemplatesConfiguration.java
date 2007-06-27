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
package org.jboss.tools.common.model.ui.templates;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.wst.xml.ui.StructuredTextViewerConfigurationXML;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author au
 */
public class TemplatesConfiguration extends StructuredTextViewerConfigurationXML {

	public IAutoEditStrategy[] getAutoEditStrategies(ISourceViewer sourceViewer, String contentType) {
		IAutoEditStrategy s = new DefaultIndentLineAutoEditStrategy(){
			private void _autoIndentAfterNewLine(IDocument d, DocumentCommand c) {
				if (c.offset == -1 || d.getLength() == 0) 
					return;
					
				try {
					// find start of line
					int p= (c.offset == d.getLength() ? c.offset  - 1 : c.offset);
					IRegion info= d.getLineInformationOfOffset(p);
					int start= info.getOffset();
						
					// find white spaces
					int end= findEndOfWhiteSpace(d, start, c.offset);
					
					StringBuffer buf= new StringBuffer(c.text);
					
					if (end > start) {			
						// append to input
						buf.append(d.get(start, end - start));
					}
					
					c.text= buf.toString();
				} catch (BadLocationException excp) {
					ModelUIPlugin.getPluginLog().logError(excp);
					// stop work
				}	
			}
			public void customizeDocumentCommand(IDocument d, DocumentCommand c) {
				try {
					if (c instanceof DocumentCommand) {
						DocumentCommand structuredDocumentCommand = (DocumentCommand)c;
						if (structuredDocumentCommand.length == 0 && structuredDocumentCommand.text != null && TextUtilities.endsWith(d.getLegalLineDelimiters(), structuredDocumentCommand.text) != -1) {
							_autoIndentAfterNewLine(d, structuredDocumentCommand);
							return;
						}
					}
				} catch (Exception x) {
					ModelUIPlugin.getPluginLog().logError("Error in customizing document command", x);
				}
				super.customizeDocumentCommand(d, c);
			}

		};
		IAutoEditStrategy[] ss = new IAutoEditStrategy[]{s};
		return ss;
	}
	
}
