package org.jboss.tools.ui.bot.ext;

/**
 * 
 * @author lzoubek
 *
 */
public class Timing {

	public static double multiplier=1.0;
	
	static {
		// TODO determine or auto set multiplier
	}
	public static int time3S() {
		return  (int) Math.round(3000*multiplier);
	}
	public static int time2S() {
		return  (int) Math.round(2000*multiplier);
	}
	public static int time10S() {
		return (int) Math.round(10000*multiplier);
	}
	public static int time20S() {
		return (int) Math.round(20000*multiplier);
	}
	public static int time5S() {
		return (int) Math.round(5000*multiplier);
	}
	public static int time500MS() {
		return (int) Math.round(500*multiplier);
	}
	public static int time1S() {
		return (int) Math.round(1000*multiplier);
	}
	public static int time(int milis) {
		return (int) Math.round(milis*multiplier);
	}
	public static long time100S() {
		return (int) Math.round(100*1000*multiplier);
	}
}
