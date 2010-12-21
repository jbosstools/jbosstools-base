/*******************************************************************************
 * Copyright (c) 2007-2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.ui.bot.ext.gef;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;


/**
 * Arranger can help to  arrange rectangular shapes on the given area
 * @author jpeterka
 *
 */
public class SWTArranger {

	private List<Block> blocks = new ArrayList<Block>();
		
	private void addDifferent(List<Block> nb) {
		for (Block n : nb ) {
			boolean found = false;
			for (Block b : blocks) {
				if (b.x == n.x && b.y == n.y && b.width == n.width && b.height == n.height) {
					found = true;
					System.out.println(String.format("ignored: %d,%d,%d,%d", b.x, b.y,b.width, b.height));
					break;
				}
			}
			if (!found) 
				blocks.add(n);	
				System.out.println(String.format("added: %d,%d,%d,%d", n.x, n.y,n.width, n.height));
			}
	}
	
	/** 
	 * Set origin canvas size
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 */	
	public void setOrigin(Rectangle rect) {
		setOrigin(rect.x, rect.y, rect.width, rect.height);
	}
	
	public void setOrigin(int x, int y, int width, int height) {
		blocks.clear();
		blocks.add(new Block(x,y,width, height));
	}
	
	/** 
	 * Put rectangle with border
	 * @param width
	 * @param height
	 */
	public void put(int width, int height) {
		put(width, height, 0);
	}
	
	/**
	 * Put rectangle on empty space
	 * @param width
	 * @param height
	 * @param border 
	 */
	public void put(int width, int height, int border) {
		boolean found = false;
		for (Block b : blocks) {
			if ((b.width >= width) && (b.height >= height)) {
				addRect(b.x + border, b.y + border, width, height);
				System.out.println(width + "," + height + " yep, i can arrange " + b.x + "," + b.y);				
				found = true;
				break;
			}			
		}
		if (!found) System.out.println("there is no free location, sorry");
	}
		
	/**
	 * Add rectangle manually (be sure not to overlapp another)
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 */
	public void addRect(int x, int y, int width, int height) {
		Point p1 = new Point();
		Point p2 = new Point();
		
		List<Block> nb = new ArrayList<Block>();
		
		for (int i = 0; i < blocks.size(); i++) {
			Block r = blocks.get(i);
			System.out.println(String.format("Trying to add: %d,%d,%d,%d", r.x, r.y,r.width, r.height));

			// full cover
			if (x <= r.x && y <= r.y && (x + width) >= (r.x  + r.width) && (y + height) >= (r.y + r.height)) {
			  r.invalid = true;
			  System.out.println(String.format("full invalid: %d,%d,%d,%d", r.x, r.y,r.x + r.width, r.y - r.height));
			}			
			// upper
			if (((y > r.y) && (y < (r.y + r.height))) && penetrateCheck(x, x+width, r.x, r.x + r.width))
			{
				p1.x = r.x;
				p1.y = r.y;
				p2.x = r.x + r.width;
				p2.y = y;	
				nb.add(new Block(p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				r.invalid = true;
				System.out.println(String.format("upper: %d,%d,%d,%d", p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				assertTrue(p1.x != p2.x );
			}
			// lower
			if ((((y + height) < (r.y + r.height)) && ((y + height) > r.y )) && penetrateCheck(x, x+width, r.x, r.x + r.width)) {
				p1.x = r.x;
				p1.y = y + height;
				p2.x = r.x + r.width;
				p2.y = r.y + r.height;			
				nb.add(new Block(p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				r.invalid = true;
				System.out.println(String.format("lower: %d,%d,%d,%d", p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				assertTrue(p1.x != p2.x );
			}
			// left
			if ((((x < (r.x + r.width)) && (x > r.x)) && penetrateCheck(y, y+height, r.y, r.y + r.height))){
				p1.x = r.x;
				p1.y = r.y;
				p2.x = x;
				p2.y = r.y + r.height;
				nb.add(new Block(p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				r.invalid = true;
				System.out.println(String.format("left: %d,%d,%d,%d", p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));				
				assertTrue(p1.x != p2.x );
			}			
			// right			
			if ((((x + width) < (r.x + r.width)) && ((x + width) > r.x)) && penetrateCheck(y, y+height, r.y, r.y + r.height)) {
				p1.x = x + width;
				p1.y = r.y;
				p2.x = r.x + r.width;
				p2.y = r.y + r.height;
				nb.add(new Block(p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				r.invalid = true;
				System.out.println(String.format("right: %d,%d,%d,%d", p1.x, p1.y,p2.x - p1.x, p2.y - p1.y));
				assertTrue(p1.x != p2.x );
			}			
		}

		for (int i = blocks.size() - 1; i >= 0; i--) {
			if (blocks.get(i).invalid) blocks.remove(i);
		}
		addDifferent(nb);
		nb.clear();
	}
	
	private  boolean penetrateCheck(int min, int max, int min2, int max2 ) {
		if (min < max2 && (max > min2)) {
			return true;
		}
		return false;
	}
	
	/**
	 * For debug purposes
	 */
	public void printBlocks() {
		System.out.println("Details");
		for (Block r: blocks) {
			System.out.println(String.format("P: %d,%d,%d,%d", r.x,r.y,r.x + r.width,r.y + r.height));
		}
	}
	
	/**
	 * Debug only
	 */
	public static void main(String[] args) {

		final SWTArranger a = new SWTArranger();
		a.setOrigin(0,0,200,200);
		for (int i = 0; i < 10; i++)
			a.addRect(i*10, i*10, 10, 10);
		a.addRect(40,40,40,40);
		a.addRect(50,50,40,40);
		a.put(40, 60);
		a.put(100, 100);
		System.out.println("Done");
		
		drawModel(a);

		System.out.println("Done-SWT");
	}
	
	/**
	 * Debug only
	 */
	private static void drawModel(final SWTArranger a) {
		final Display d = new Display();
		Shell s = new Shell(d);
		s.setSize(500,500);		
		
		s.setLayout(new FillLayout());
		
		Canvas canvas = new Canvas(s, SWT.NONE);
		canvas.setSize(500,500);
		canvas.setLocation(20,20);
		canvas.addPaintListener(new PaintListener() {

			@Override
			public void paintControl(PaintEvent e) {
				Color c = new Color(d,0x00, 0x00, 0x00);
				GC gc = e.gc;
				gc.setBackground(c);
				int i = 0;
				for (Block r : a.blocks) {
					i += 10;
					gc.fillRectangle(r.x, r.y, r.width, r.height);
				}
				c.dispose();
				gc.dispose();
			}			
		});
		s.pack();
		s.open();

		while (!s.isDisposed()) {
			if (!d.readAndDispatch()) {
				d.sleep();
			}
		}
		d.dispose();	
	}
	
	/**
	 * Block, just a simple rectangle
	 * @author jpeterka
	 *
	 */
	class Block {
		public boolean invalid = false;
		
		public int x,y,width,height;
			
		public Block(int x, int y, int width, int height) {
			this.x = x;
			this.y = y;
			this.width = width;
			this.height = height;
		}
	}
}
