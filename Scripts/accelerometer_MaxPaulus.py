import json
import numpy as np

from pathlib import Path
from pandas import read_csv
from scipy import signal

from typing import List, Tuple, Dict
from numpy.typing import NDArray


class Acceleration:
    raw_data: List[Tuple]
    timemap: Dict
    time: NDArray[np.float64]
    timestamps: NDArray[np.int64]
    intensities: NDArray[np.float64]
    unfiltered: NDArray[np.float64]
    events: Dict

    def __init__(self, parent_folder: str, subject_folder: str):
        self.raw_data, self.timemap, self.events = self._read_from_folder(parent_folder, subject_folder)
        self.intensities, self.unfiltered, self.timestamps = self._create_signal(self.raw_data)
        self.time = np.vectorize(self._local2utc)(self.timestamps, local=self.timemap["local"], utc=self.timemap["utc"])        

    def _read_from_folder(self, parent_folder: str, subject_folder: str):
        path = Path(parent_folder) / Path(subject_folder)
        for p in path.iterdir():
            if p.suffix == ".json":
                data = self._read_data(p)
            elif p.suffix == ".txt":
                if "timemap" in p.stem:
                    timemap = self._read_timemap(p)
                elif "marker" in p.stem:
                    events = self._read_markers(p)
        return data, timemap, events

    def _read_data(self, path):
        with open(path, "r") as f:
            data = (json.loads(f.read()))
            data = [sample["MeasIMU9"] for sample in data["Samples"] if "MeasIMU9" in sample.keys()]
            data = [(sample["Timestamp"], sample["ArrayAcc"]) for sample in data]
            return data
    
    def _read_timemap(self, path):
        with open(path, "r") as f:
            tmdf = read_csv(f)
            tmdf.columns = tmdf.columns.str.strip()
            return {
                "utc": tmdf["Device UTC (us)"][0], 
                "local": tmdf["Device local (ms)"][0]
            }

    def _read_markers(self, path):
        with open(path, "r") as f:
            markers = read_csv(f, header=None)
            names = markers.iloc[:,1].str.strip().values
            count = {}
            new_names = []
            for name in names:
                if name in count:
                    count[name] += 1
                    new_names.append(f"{name}{count[name]}")
                else:
                    count[name] = 1
                    new_names.append(name)
            times = (markers.iloc[:,0] * 1000).values  # s to ms
            return {
                "name": new_names,
                "time": times
            }

    def _create_signal(self, data, fs=208, n_samples=8):
        period = 1000 / fs
        n_expected = n_samples
        datapoints_xyz = {"x": [], "y": [], "z": []}
        timepoints = []
        for datapoint in data:
            ts0, samples = datapoint
            n = len(samples)
            if n != n_expected:
                print(n, "samples")
                # TODO interpolate
            for sample in samples:
                for k in datapoints_xyz:
                    datapoints_xyz[k].append(sample[k])
            timepoints.extend([ts0 + i * period for i in range(len(samples))])
        intensity_unfiltered = np.sqrt(np.add(*[np.array(a)**2 for a in datapoints_xyz.values()]))
        # filtering
        # we can obtain magnitude either before (1) or after filtering (2)
        # since the high-pass filter centers the signal around 0, (2) will not contain oscillations
        #intensity_filtered = self._filt_norm_signal(intensity_unfiltered, fs)
        filtered_xyz = [self._filt_norm_signal(np.array(datapoints_xyz[k]), fs) for k in datapoints_xyz]
        s = np.sqrt(np.add(*[a**2 for a in filtered_xyz]))
        # normalize
        #s = (s-np.min(s)) / (np.max(s)-np.min(s)) 
        
        return s, intensity_unfiltered, np.array(timepoints)

    @staticmethod
    def _filt_norm_signal(s, fs):
        nyquist = 0.5 * fs
        # high-pass: gravity
        order = 4
        cutoff_freq = 0.5
        normalized_cutoff = cutoff_freq / nyquist
        b, a = signal.butter(order, normalized_cutoff, btype='high', analog=False)
        s = signal.filtfilt(b, a, s)
        # low-pass: noise
        order = 4
        cutoff_freq = 10
        normalized_cutoff = cutoff_freq / nyquist
        b, a = signal.butter(order, normalized_cutoff, btype='low', analog=False)
        s = signal.filtfilt(b, a, s)

        return s
    
    @staticmethod
    def _local2utc(timestamp, local, utc):
        new_time = timestamp - local + (utc / 1000)
        return new_time
    