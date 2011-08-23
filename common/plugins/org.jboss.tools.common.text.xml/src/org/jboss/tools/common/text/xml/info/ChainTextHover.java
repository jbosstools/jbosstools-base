/******************************************************************************* 
 * Copyright (c) 2009-2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.text.xml.info;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.wst.sse.ui.internal.Logger;
import org.eclipse.wst.sse.ui.internal.taginfo.AnnotationHoverProcessor;
import org.eclipse.wst.sse.ui.internal.taginfo.DebugInfoHoverProcessor;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;
import org.jboss.tools.common.text.xml.xpl.MarkerProblemAnnotationHoverProcessor;


/**
 * Provides the best hover help documentation (by using other hover help
 * processors) Priority of hover help processors is: ProblemHoverProcessor,
 * FaceletTagInfoProcessor, TagInfoProcessor, AnnotationHoverProcessor
 * 
 * The processors are acquired in order of their priorities. If a hover doesn'n returns an information 
 * (i.e. returns null as a display string) the next processor will be acquired.
 *
 * @author Victor Rubezhny
 *
 */
@SuppressWarnings("restriction")
public class ChainTextHover implements ITextHover, ITextHoverExtension, ITextHoverExtension2 {
	private ITextHover fBestMatchHover; // current best match text hover
	private ITextHover[] fTagInfoHovers; // documentation/information hover
	private List<ITextHover> fTextHovers; // list of text hovers to consider in best
	// match

	public ChainTextHover(ITextHover infoTagHover) {
		this(new ITextHover[]{infoTagHover});
	}

	public ChainTextHover(ITextHover[] infoTagHovers) {
		fTagInfoHovers = infoTagHovers;
	}

	/**
	 * Create a list of text hovers applicable to this best match hover
	 * processor
	 * 
	 * @return List of ITextHover - in abstract class this is empty list
	 */
	private List<ITextHover> createTextHoversList() {
		List<ITextHover> hoverList = new ArrayList<ITextHover>();
		// if currently debugging, then add the debug hover to the list of
		// best match
		if (Logger.isTracing(DebugInfoHoverProcessor.TRACEFILTER)) {
			hoverList.add(new DebugInfoHoverProcessor());
		}

//		hoverList.add(new ProblemAnnotationHoverProcessor());
		hoverList.add(new MarkerProblemAnnotationHoverProcessor());
		int lastToolsIndex = hoverList.size();
		for (int i = 0; i < fTagInfoHovers.length; i++) {
			if (fTagInfoHovers[i].getClass().getName().startsWith("org.jboss.tools.")) { //$NON-NLS-1$
				hoverList.add(lastToolsIndex,fTagInfoHovers[i]);
				lastToolsIndex++;
			} else {
				hoverList.add(fTagInfoHovers[i]);
			}
		}
		hoverList.add(new AnnotationHoverProcessor());
		return hoverList;
	}

	public IInformationControlCreator getHoverControlCreator() {
		IInformationControlCreator creator = null;

		if (fBestMatchHover instanceof ITextHoverExtension) {
			creator = ((ITextHoverExtension) fBestMatchHover).getHoverControlCreator();
		}
		return creator;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.ITextHover#getHoverInfo(org.eclipse.jface.text.ITextViewer,
	 *      org.eclipse.jface.text.IRegion)
	 */
	public String getHoverInfo(ITextViewer viewer, IRegion hoverRegion) {
		String displayInfo = null;

		// already have a best match hover picked out from getHoverRegion call
		if (fBestMatchHover != null) {
				displayInfo = fBestMatchHover.getHoverInfo(viewer, hoverRegion);
		}
		// either had no best match hover or best match hover returned null
		if (displayInfo == null) {
			// go through the list of text hovers and return first display string
			Iterator<ITextHover> i = getTextHovers().iterator();
			while ((i.hasNext()) && (displayInfo == null)) {
				ITextHover hover = (ITextHover) i.next();
				if(hover instanceof ITextHoverExtension2) {
					Object displayInfoObject = ((ITextHoverExtension2)hover).getHoverInfo2(viewer, hoverRegion);
					if(displayInfoObject!=null) {
						displayInfo = displayInfoObject.toString();
					}
				} else {
					displayInfo = hover.getHoverInfo(viewer, hoverRegion);
				}
			}
		}
		return displayInfo;
	}
	
	public Object getHoverInfo2(ITextViewer viewer, IRegion hoverRegion) {
		Object objectInfo = null;

		// already have a best match hover picked out from getHoverRegion call
		if (fBestMatchHover != null) {
			if (fBestMatchHover instanceof ITextHoverExtension2) {
				objectInfo = ((ITextHoverExtension2) fBestMatchHover).getHoverInfo2(viewer, hoverRegion);
			} else {
				objectInfo = fBestMatchHover.getHoverInfo(viewer, hoverRegion);
			}
		}
		if (objectInfo == null) {
			// go through the list of text hovers and return first display string
			Iterator<ITextHover> i = getTextHovers().iterator();
			while ((i.hasNext()) && (objectInfo == null)) {
				ITextHover hover = (ITextHover) i.next();
				if(hover instanceof ITextHoverExtension2) {
					objectInfo = ((ITextHoverExtension2)hover).getHoverInfo2(viewer, hoverRegion);
				} else {
					objectInfo = hover.getHoverInfo(viewer, hoverRegion);
				}
			}
		}
		// either had no best match hover or best match hover returned null
		return objectInfo;
	}

	/* 
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.text.ITextHover#getHoverRegion(org.eclipse.jface.text.ITextViewer,
	 *      int)
	 */
	public IRegion getHoverRegion(ITextViewer viewer, int offset) {
		IRegion hoverRegion = null;

		// go through list of text hovers and return first hover region
		ITextHover hover = null;
		Iterator<ITextHover> i = getTextHovers().iterator();
		while ((i.hasNext()) && (hoverRegion == null)) {
			hover = i.next();
			hoverRegion = hover.getHoverRegion(viewer, offset);
		}

		// store the text hover processor that found region
		if (hoverRegion != null)
			fBestMatchHover = hover;
		else
			fBestMatchHover = null;

		return hoverRegion;
	}

	private List<ITextHover> getTextHovers() {
		if (fTextHovers == null) {
			fTextHovers = createTextHoversList();
		}
		return fTextHovers;
	}
}
